{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinition (definitionRequestToResponse) where

import Data.Int
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (Uri, uriToFilePath, getUri, _line, _character )
import Data.Text as T
import Data.List as List
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (decodeMessage)
import System.FilePath.Find as Find
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (makeRelative)

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (documents, occurrences, role, symbol, startLine, endLine, startCharacter, endCharacter, range, uri)
import LSP

--TODO: use Monad transformer

definitionRequestToResponse :: DefinitionRequest -> IO DefinitionResponse
definitionRequestToResponse definitionRequest = do
  maybeLocation <- findLocationFromRequest definitionRequest
  return $ definitionResponse definitionRequest maybeLocation

findLocationFromRequest :: DefinitionRequest -> IO (Maybe L.Location)
findLocationFromRequest definitionRequest = do
  let pos = definitionRequest^.LSPLens.params^.LSPLens.position
  let uri = definitionRequest^.LSPLens.params^.LSPLens.textDocument^.LSPLens.uri
  -- find the `TextDocument` of the request
  maybeTextDocument <- textDocumentWthUri uri
  case maybeTextDocument of
    Nothing -> return $ Nothing
    -- find the symbol in the `TextDocument` at the given `Position`
    Just textDocument -> case occurrenceAtPosition pos textDocument of
      Nothing -> return $ Nothing
      Just symbolOccurence ->
        case symbolOccurence^.role of
          S.SymbolOccurrence'UNKNOWN_ROLE -> return Nothing
          -- the symbol is already the definition itself
          S.SymbolOccurrence'DEFINITION -> return $ Just $ lspLocation uri symbolOccurence
          -- the symbol is a reference, we search first if it's defined in the same file
          S.SymbolOccurrence'REFERENCE ->  case definitionInTexDocument symbolOccurence textDocument of
            -- yes, we can directly return the location
            Just definitionSymbol -> return $ Just $ lspLocation uri definitionSymbol
            Nothing -> do
              -- No, we'll try optimistic search, maybe the file has the same name as the symbol
              maybeOptimistResult <- defnitionWithOptimisticSearch symbolOccurence
              case maybeOptimistResult of
                Just symbolWithTextDocument -> responseFromSymbolWithTextDocument symbolWithTextDocument
                Nothing -> do
                  -- No, we have to search in all project files for the definition.
                  maybeResult <- defnitionInProjectFiles symbolOccurence
                  case maybeResult of
                    Nothing -> return Nothing
                    Just symbolWithTextDocument -> responseFromSymbolWithTextDocument  symbolWithTextDocument


responseFromSymbolWithTextDocument :: (S.SymbolOccurrence, S.TextDocument) -> IO (Maybe L.Location)
responseFromSymbolWithTextDocument symbolWithTextDocument = do
  let definitionSymbol = fst symbolWithTextDocument
  let definitionTexDocument = snd symbolWithTextDocument
  definitionUri <- uriFromTextDocument definitionTexDocument
  return $ Just $ lspLocation definitionUri definitionSymbol


textDocumentWthUri :: Uri -> IO (Maybe S.TextDocument)
textDocumentWthUri uri = do
  currentDirectory <- getCurrentDirectory
  let maybeFilePath = fmap (makeRelative currentDirectory) (uriToFilePath uri)
  case maybeFilePath of
    Nothing -> return Nothing
    Just filePath -> do
      allFiles <- listAllfiles
      inner filePath allFiles
      where
        inner :: FilePath -> [FilePath] -> IO (Maybe S.TextDocument)
        inner filePath files =
          case files of
            [] -> return Nothing
            x:tail -> do
              maybeRes <- textDocumentFromUriAndSemanticdbFile filePath x
              case maybeRes of
                Nothing -> inner filePath tail
                Just res -> return $ Just res

occurrenceAtPosition :: Position -> S.TextDocument -> Maybe S.SymbolOccurrence
occurrenceAtPosition position textDocument = List.find (\symbol -> isPosititionInRange position (symbol^.range)) (textDocument^.occurrences)


definitionInTexDocument :: S.SymbolOccurrence -> S.TextDocument -> Maybe S.SymbolOccurrence
definitionInTexDocument symbolOccurence textDocument =
  case (symbolOccurence^.role) of
    S.SymbolOccurrence'UNKNOWN_ROLE -> Nothing
    S.SymbolOccurrence'DEFINITION -> Just symbolOccurence
    S.SymbolOccurrence'REFERENCE ->
      List.find (\symb -> symb^.symbol == symbolOccurence^.symbol && symb^.role == S.SymbolOccurrence'DEFINITION) (textDocument^.occurrences)


defnitionWithOptimisticSearch :: S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
defnitionWithOptimisticSearch symbolOccurence = do
  findFile (extractFileName symbolOccurence) >>= searchOccurence symbolOccurence
  where
  findFile fileName = getCurrentDirectory >>= Find.find always (Find.fileName ==? fileName)
  extractFileName :: S.SymbolOccurrence -> FilePath
  extractFileName symbolOccurence =
    let
      takeName = List.takeWhile (\char -> not (char == '.' || char == '#'))
      dropPrefixPath = List.reverse . (List.takeWhile (\char -> not (char == '/' ))) . List.reverse
    in
      (takeName . dropPrefixPath $ T.unpack $ (symbolOccurence^.symbol)) ++ ".scala.semanticdb"


defnitionInProjectFiles :: S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
defnitionInProjectFiles symbolOccurence = do
  listAllfiles >>= searchOccurence symbolOccurence


searchOccurence :: S.SymbolOccurrence -> [FilePath]  -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
searchOccurence symbol [] = return Nothing
searchOccurence symbol (filePath:tail) = do
  maybeRes <- findOccurenceFromFilePath filePath symbol
  case maybeRes of
    Nothing -> searchOccurence symbol tail
    Just res -> return $ Just res


findOccurenceFromFilePath :: FilePath -> S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
findOccurenceFromFilePath filePath symbol = do
  maybeTextDocuments <- textDocumentsFromFilePath filePath
  return $ maybeTextDocuments >>= occurenceInTextDocuments symbol . view documents


occurenceInTextDocuments :: S.SymbolOccurrence -> [S.TextDocument] -> Maybe (S.SymbolOccurrence, S.TextDocument)
occurenceInTextDocuments _ [] = Nothing
occurenceInTextDocuments symbol (textDocument:tail) =
  case definitionInTexDocument symbol textDocument of
    Nothing -> occurenceInTextDocuments symbol tail
    Just res -> Just $ (res, textDocument)


listAllfiles :: IO([FilePath])
listAllfiles = getCurrentDirectory >>= Find.find always (extension ==? ".semanticdb")


uriFromTextDocument :: S.TextDocument -> IO (Uri)
uriFromTextDocument textDocument = do
  currentDirectory  <- getCurrentDirectory
  return $ Uri (T.pack( ("file://" ++ currentDirectory ++ "/" ++ (T.unpack $ textDocument^.uri))))


semanticdbRangeToLSPRange :: S.Range -> L.Range
semanticdbRangeToLSPRange sRange =
  let
    startPosition = L.Position (int32ToInt (sRange^.startLine)) (int32ToInt (sRange^.startCharacter))
    endPosition = L.Position (int32ToInt (sRange^.endLine)) (int32ToInt (sRange^.endCharacter))
  in Range startPosition endPosition



lspLocation :: Uri -> S.SymbolOccurrence -> L.Location
lspLocation uri symbolOccurence =
  L.Location
    uri
    (semanticdbRangeToLSPRange (symbolOccurence^.range))


textDocumentsFromFilePath :: FilePath -> IO (Maybe S.TextDocuments)
textDocumentsFromFilePath filePath = do
  message <- BS.readFile filePath
  return $ case decodeMessage message of
    Left e -> Nothing
    Right msg -> Just msg


decodeTextDocuments :: BS.ByteString -> Maybe S.TextDocuments
decodeTextDocuments message =
  case decodeMessage message of
    Left e -> Nothing
    Right msg -> Just msg


textDocumentFromUriAndSemanticdbFile :: FilePath -> FilePath -> IO(Maybe S.TextDocument)
textDocumentFromUriAndSemanticdbFile uri semanticdbFile = do
  message <- BS.readFile semanticdbFile
  return $ decodeTextDocuments message >>=  textDocumentWithUri uri


isSameUri :: FilePath -> S.TextDocument -> Bool
isSameUri filePath textDocument =  (T.unpack $ textDocument^.uri) == filePath


textDocumentWithUri :: FilePath -> S.TextDocuments -> Maybe S.TextDocument
textDocumentWithUri uri textDocuments = List.find (isSameUri uri) $ textDocuments ^.documents


isPosititionInRange :: Position -> S.Range -> Bool
isPosititionInRange position range =
     _line position >= int32ToInt  (range^.startLine)
  && _line position <= int32ToInt  (range^.endLine)
  && _character position >= int32ToInt  (range^.startCharacter)
  && _character position <= int32ToInt  (range^.endCharacter)


int32ToInt :: Int32 -> Int
int32ToInt int32 = fromIntegral int32 :: Int
