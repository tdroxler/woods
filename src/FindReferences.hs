{-# LANGUAGE TupleSections #-}

module FindReferences (referenceRequestToResponse)  where

import Data.Traversable (traverse)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import qualified Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (ReferencesRequest, ReferencesResponse, )
import Data.List as List

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (occurrences, role, symbol)
import LSP
import Helpers


referenceRequestToResponse ::  ReferencesRequest -> IO ReferencesResponse
referenceRequestToResponse referenceRequest =
  referencesResponse referenceRequest <$> findLocationsFromRequest referenceRequest


findLocationsFromRequest :: ReferencesRequest -> IO [L.Location]
findLocationsFromRequest referenceRequest = do
  let pos = referenceRequest^.(LSPLens.params . LSPLens.position)
  let uri = referenceRequest^.(LSPLens.params . LSPLens.textDocument . LSPLens.uri)
  -- find the `TextDocument` of the request
  maybeTextDocument <- textDocumentWthUri uri
  case maybeTextDocument of
    Nothing -> return []
    Just textDocument -> case occurrenceAtPosition pos textDocument of
      Nothing -> return []
      Just symbolOccurence ->
        case symbolOccurence^.role of
          S.SymbolOccurrence'UNKNOWN_ROLE -> return []
          _ -> do
            allSymbols <- findReferencesInProjectFiles symbolOccurence
            traverse locationFromSymbolWithTextDocument allSymbols


findReferencesInProjectFiles :: S.SymbolOccurrence -> IO [(S.SymbolOccurrence, S.TextDocument)]
findReferencesInProjectFiles symbolOccurence = do
  textDocuments <- listAllfiles >>= traverse  listTextDocumentFromFilePath
  return $ List.concat textDocuments >>=  sameSymbolsInTexDocument symbolOccurence


sameSymbolsInTexDocument :: S.SymbolOccurrence -> S.TextDocument -> [(S.SymbolOccurrence, S.TextDocument)]
sameSymbolsInTexDocument symbolOccurence textDocument =
  fmap (, textDocument) (List.filter (\symb -> symb^.symbol == symbolOccurence^.symbol) (textDocument^.occurrences))
