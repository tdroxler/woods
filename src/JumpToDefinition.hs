{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinition (definitionRequestToResponse) where

import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import qualified Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (DefinitionRequest, DefinitionResponse, Uri, uriToFilePath, getUri, _line, _character )
import Data.Text as T
import Data.List as List
import System.FilePath.Find as Find
import System.Directory (getCurrentDirectory)

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (occurrences, role, symbol)
import LSP
import Helpers


definitionRequestToResponse :: DefinitionRequest -> IO DefinitionResponse
definitionRequestToResponse definitionRequest =
  definitionResponse definitionRequest <$> findLocationFromRequest definitionRequest

findLocationFromRequest :: DefinitionRequest -> IO ([L.Location])
findLocationFromRequest definitionRequest = do
  let pos = definitionRequest ^. (LSPLens.params . LSPLens.position)
  let uri = definitionRequest ^. (LSPLens.params . (LSPLens.textDocument . LSPLens.uri))
  -- find the `TextDocument` of the request
  maybeTextDocument <- textDocumentWthUri uri
  case maybeTextDocument of
    Nothing -> return []
    -- find the symbol in the `TextDocument` at the given `Position`
    Just textDocument -> case occurrenceAtPosition pos textDocument of
      Nothing -> return []
      Just symbolOccurence -> do
        case symbolOccurence^.role of
          S.SymbolOccurrence'UNKNOWN_ROLE -> return []
          -- the symbol is already the definition itself
          S.SymbolOccurrence'DEFINITION -> return $ [lspLocation uri symbolOccurence]
          -- the symbol is a reference, we search first if it's defined in the same file
          S.SymbolOccurrence'REFERENCE ->  case definitionSymbolInTextDocument symbolOccurence textDocument of
            -- yes, we can directly return the location
            Just definitionSymbol -> return $ [lspLocation uri definitionSymbol]
            Nothing -> do
              -- No, we'll try optimistic search, maybe the file has the same name as the symbol
              maybeOptimistResult <- defnitionWithOptimisticSearch symbolOccurence
              case maybeOptimistResult of
                Just symbolWithTextDocument -> return <$> locationFromSymbolWithTextDocument symbolWithTextDocument
                Nothing -> do
                  -- No, we have to search in all project files for the definition.
                  maybeResult <- findDefinitionInProjectFiles symbolOccurence
                  case maybeResult of
                    Nothing -> return []
                    Just symbolWithTextDocument -> return <$> locationFromSymbolWithTextDocument symbolWithTextDocument


defnitionWithOptimisticSearch :: S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
defnitionWithOptimisticSearch symbolOccurence =
  findFile (extractFileName symbolOccurence) >>= definitionFromFilePathes symbolOccurence
  where
  findFile fileName = getCurrentDirectory >>= Find.find always (Find.fileName ==? fileName)
  extractFileName :: S.SymbolOccurrence -> FilePath
  extractFileName symbolOccurence =
    let
      takeName = List.takeWhile (\char -> not (char == '.' || char == '#'))
      dropPrefixPath = List.reverse . List.takeWhile (/= '/') . List.reverse
    in
      (takeName . dropPrefixPath $ T.unpack (symbolOccurence^.symbol)) ++ ".scala.semanticdb"


findDefinitionInProjectFiles :: S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
findDefinitionInProjectFiles symbolOccurence =
  listAllfiles >>= definitionFromFilePathes symbolOccurence


definitionFromFilePathes :: S.SymbolOccurrence -> [FilePath]  -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
definitionFromFilePathes symbol [] = return Nothing
definitionFromFilePathes symbol (filePath:tail) = do
  maybeRes <- definitionInTextDocuments symbol <$> listTextDocumentFromFilePath filePath
  case maybeRes of
    Nothing -> definitionFromFilePathes symbol tail
    Just res -> return $ Just res


definitionInTextDocuments :: S.SymbolOccurrence -> [S.TextDocument] -> Maybe (S.SymbolOccurrence, S.TextDocument)
definitionInTextDocuments _ [] = Nothing
definitionInTextDocuments symbol (textDocument:tail) =
  case definitionSymbolInTextDocument symbol textDocument of
    Nothing -> definitionInTextDocuments symbol tail
    Just res -> Just (res, textDocument)


definitionSymbolInTextDocument :: S.SymbolOccurrence -> S.TextDocument -> Maybe S.SymbolOccurrence
definitionSymbolInTextDocument symbolOccurence textDocument =
  case symbolOccurence^.role of
    S.SymbolOccurrence'UNKNOWN_ROLE -> Nothing
    S.SymbolOccurrence'DEFINITION -> Just symbolOccurence
    S.SymbolOccurrence'REFERENCE ->
      List.find (\symb -> symb^.symbol == symbolOccurence^.symbol && symb^.role == S.SymbolOccurrence'DEFINITION) (textDocument^.occurrences)
