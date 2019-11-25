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
import Helpers


definitionRequestToResponse :: DefinitionRequest -> IO DefinitionResponse
definitionRequestToResponse definitionRequest =
  definitionResponse definitionRequest <$> findLocationFromRequest definitionRequest

findLocationFromRequest :: DefinitionRequest -> IO (Maybe L.Location)
findLocationFromRequest definitionRequest = do
  let pos = definitionRequest ^. (LSPLens.params . LSPLens.position)
  let uri = definitionRequest ^. (LSPLens.params . (LSPLens.textDocument . LSPLens.uri))
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
          Just symbolWithTextDocument -> do
            print maybeDefinition
            Just <$> locationFromSymbolWithTextDocument  symbolWithTextDocument
