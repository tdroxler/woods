{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinition (definitionRequestToResponse) where

import Data.Int
import Lens.Micro
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (Uri, uriToFilePath, getUri, _line, _character )
import Data.Text as T
import Data.List as List
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (decodeMessage)

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (documents, occurrences, role, symbol, startLine, endLine, startCharacter, endCharacter, range, uri)
import LSP


definitionRequestToResponse :: DefinitionRequest -> IO DefinitionResponse
definitionRequestToResponse definitionRequest = do
  let pos = definitionRequest^.LSPLens.params^.LSPLens.position
  let lspUri = definitionRequest^.LSPLens.params^.LSPLens.textDocument^.LSPLens.uri
  maybeSymbol <- occurenceFromPositionAndUri pos lspUri
  let maybeLocation = fmap (lspLocation lspUri) maybeSymbol
  return $ definitionResponse definitionRequest maybeLocation


occurenceFromPositionAndUri :: Position -> Uri -> IO (Maybe S.SymbolOccurrence)
occurenceFromPositionAndUri position uri = do
  maybeMessage <- messageFromUri uri
  return $ case maybeMessage of
    Nothing -> Nothing
    Just textDocuments ->
      case (findSymbolOoccurence position uri textDocuments) of
        Nothing -> Nothing
        Just refSymb -> findDefinitionSymbol refSymb (textDocuments ^.documents >>= \docs -> docs ^.occurrences)


addMetaInf :: FilePath -> FilePath
addMetaInf path = (T.unpack $ replace "src/" "target/scala-2.12/classes/META-INF/semanticdb/src/" (T.pack path)) ++ ".semanticdb"


uriToSemanticdbFile :: Uri -> Maybe FilePath
uriToSemanticdbFile uri = fmap addMetaInf $ uriToFilePath uri


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


messageFromFilePath :: FilePath -> IO (Maybe S.TextDocuments)
messageFromFilePath filePath = do
  message <- BS.readFile filePath
  return $ case decodeMessage message of
    Left e -> Nothing
    Right msg -> Just msg


messageFromUri :: Uri -> IO (Maybe S.TextDocuments)
messageFromUri uri = do
  let maybePath = uriToSemanticdbFile uri
  case maybePath of
    Just filePath -> do
      messageFromFilePath filePath
    Nothing -> return Nothing


findSymbolOoccurence :: Position -> Uri -> TextDocuments -> Maybe S.SymbolOccurrence
findSymbolOoccurence position uri textDocuments =
      textDocumentWithUri uri textDocuments >>= findOccurrenceAtPosition position


isSameUri :: Uri -> S.TextDocument -> Bool
isSameUri lspUri textDocument =  List.isSuffixOf (T.unpack $ textDocument^.uri) (T.unpack $ getUri lspUri)


textDocumentWithUri :: Uri -> S.TextDocuments -> Maybe S.TextDocument
textDocumentWithUri uri textDocuments = List.find (isSameUri uri) $ textDocuments ^.documents


isPosititionInRange :: Position -> S.Range -> Bool
isPosititionInRange position range =
     _line position >= int32ToInt  (range^.startLine)
  && _line position <= int32ToInt  (range^.endLine)
  && _character position >= int32ToInt  (range^.startCharacter)
  && _character position <= int32ToInt  (range^.endCharacter)


findOccurrenceAtPosition :: Position -> S.TextDocument -> Maybe S.SymbolOccurrence
findOccurrenceAtPosition position textDocument = List.find (\symbol -> isPosititionInRange position (symbol^.range)) (textDocument^.occurrences)


findDefinitionSymbol :: S.SymbolOccurrence -> [S.SymbolOccurrence]-> Maybe S.SymbolOccurrence
findDefinitionSymbol symbolOccurence symbols =
  case (symbolOccurence ^. role) of
    S.SymbolOccurrence'UNKNOWN_ROLE -> Nothing
    S.SymbolOccurrence'DEFINITION -> Just symbolOccurence
    S.SymbolOccurrence'REFERENCE ->
      List.find (\symb -> symb^.symbol == symbolOccurence^.symbol && symb^.role == S.SymbolOccurrence'DEFINITION) symbols


int32ToInt :: Int32 -> Int
int32ToInt int32 = fromIntegral int32 :: Int
