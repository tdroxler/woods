{-# LANGUAGE OverloadedStrings #-}
module Hover (hoverRequestToResponse) where

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
import Proto.Semanticdb_Fields (symbols, symbol, uri)
import LSP
import Helpers


hoverRequestToResponse :: HoverRequest -> IO HoverResponse
hoverRequestToResponse hoverRequest =
  hoverResponse hoverRequest <$> findLocationFromRequest hoverRequest

findLocationFromRequest :: HoverRequest -> IO (Maybe Hover)
findLocationFromRequest hoverRequest = do
  let pos = hoverRequest ^. (LSPLens.params . LSPLens.position)
  let uri = hoverRequest ^. (LSPLens.params . (LSPLens.textDocument . LSPLens.uri))
  -- find the `TextDocument` of the request
  maybeTextDocument <- textDocumentWthUri uri
  case maybeTextDocument of
    Nothing -> return Nothing
    -- find the symbol in the `TextDocument` at the given `Position`
    Just textDocument -> case occurrenceAtPosition pos textDocument of
      Nothing -> return Nothing
      Just symbolOccurence -> do
        maybeDefinition <- findDefinition symbolOccurence textDocument
        case maybeDefinition of
          Nothing -> return Nothing
          Just symbolWithTextDocument ->
            return $ hoverFromSymbolWithTextDocument symbolWithTextDocument


hoverFromSymbolWithTextDocument :: (S.SymbolOccurrence, S.TextDocument) -> Maybe Hover
hoverFromSymbolWithTextDocument symbolWithTextDocument =
  let
    definitionSymbol = fst symbolWithTextDocument
    definitionTexDocument = snd symbolWithTextDocument
  in
  symbolToHover <$> informationSymbolInTextDocument definitionSymbol definitionTexDocument


informationSymbolInTextDocument :: S.SymbolOccurrence -> S.TextDocument -> Maybe S.SymbolInformation
informationSymbolInTextDocument symbolOccurence textDocument =
      List.find (\symb -> symb^.symbol == symbolOccurence^.symbol) (textDocument^.symbols)


symbolToHover :: S.SymbolInformation -> Hover
symbolToHover symbol = Hover (HoverContents (markedUpContent "scala" (T.pack $ show symbol))) Nothing

