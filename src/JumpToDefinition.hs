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
import System.FilePath.Find as Find
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (makeRelative)

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (documents, occurrences, role, symbol, startLine, endLine, startCharacter, endCharacter, range, uri)
import LSP


definitionRequestToResponse :: DefinitionRequest -> IO DefinitionResponse
definitionRequestToResponse definitionRequest = do
  let pos = definitionRequest^.LSPLens.params^.LSPLens.position
  let lspUri = definitionRequest^.LSPLens.params^.LSPLens.textDocument^.LSPLens.uri
  currentDirectory <- getCurrentDirectory
  maybeSymbol <- occurenceFromPositionAndUri currentDirectory pos lspUri
  let maybeLocation = maybeSymbol >>= (\symbol -> fmap (\x -> lspLocation x (fst symbol)) (uriFromCurrentDirAndTextDocument currentDirectory (snd symbol)))
  return $ definitionResponse definitionRequest maybeLocation


searchOccurence :: [FilePath] -> S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
searchOccurence filePathes symbol =
  case filePathes of
    [] -> return Nothing
    filePath : tail -> do
      maybeRes <- findOccurenceFromFilePath filePath symbol
      case maybeRes of
        Nothing -> searchOccurence tail symbol
        Just res -> return $ Just res


findOccurenceFromFilePath :: FilePath -> S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
findOccurenceFromFilePath filePath symbol = do
  maybeTextDocuments <- textDocumentsFromFilePath filePath
  case maybeTextDocuments of
    Nothing -> return Nothing
    Just textDocuments -> return $ occurenceInTextDocuments symbol (textDocuments^.documents)


occurenceInTextDocuments :: S.SymbolOccurrence -> [S.TextDocument] -> Maybe (S.SymbolOccurrence, S.TextDocument)
occurenceInTextDocuments symbol textDocuments =
  case textDocuments of
    [] -> Nothing
    textDocument : tail ->
      case occurenceInTextDocument symbol textDocument of
        Nothing -> occurenceInTextDocuments symbol tail
        Just res -> Just $ (res, textDocument)


occurenceInTextDocument :: S.SymbolOccurrence -> S.TextDocument -> Maybe S.SymbolOccurrence
occurenceInTextDocument symbol textDocument =
  findDefinitionSymbol symbol (textDocument^.occurrences)


-- TODO: all the optimization could come from here and the order of the semantidb files
--       as we search in one file after the other the occurence
listAllfiles :: FilePath ->IO([FilePath])
listAllfiles start = Find.find always (extension ==? ".semanticdb") start


occurenceFromPositionAndUri :: FilePath -> Position -> Uri -> IO (Maybe (S.SymbolOccurrence, S.TextDocument))
occurenceFromPositionAndUri currentDirectory position uri = do
  maybeMessage <- messageFromUri currentDirectory uri
  case maybeMessage of
    Nothing -> return Nothing
    Just textDocument ->
      case (findSymbolOoccurence position textDocument) of
        Nothing -> return Nothing
        Just refSymb -> do
          case refSymb ^. role of
            S.SymbolOccurrence'DEFINITION -> return $ Just (refSymb, textDocument)
            _ -> do
              filePathes <- listAllfiles currentDirectory
              searchOccurence filePathes refSymb


uriFromCurrentDirAndTextDocument :: FilePath -> S.TextDocument -> Maybe Uri
uriFromCurrentDirAndTextDocument currentDirectory textDocument =
  Just $ Uri (T.pack( ("file://" ++ currentDirectory ++ "/" ++ (T.unpack $ textDocument^.uri))))


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

textDocumentsForUri :: FilePath -> IO (Maybe S.TextDocument)
textDocumentsForUri filePath = do
  allFiles <- getCurrentDirectory >>= listAllfiles
  inner filePath allFiles
  where
    inner :: FilePath -> [FilePath] -> IO (Maybe S.TextDocument)
    inner filePath files =
      case files of
        [] -> return Nothing
        x:tail -> do
          maybeRes <- textDocumentFrom filePath x
          case maybeRes of
            Nothing -> inner filePath tail
            Just res -> return $ Just res


decodeTextDocuments :: BS.ByteString -> Maybe S.TextDocuments
decodeTextDocuments message =
  case decodeMessage message of
    Left e -> Nothing
    Right msg -> Just msg


textDocumentFrom :: FilePath -> FilePath -> IO(Maybe S.TextDocument)
textDocumentFrom uri semanticdbFile = do
  message <- BS.readFile semanticdbFile
  return $ decodeTextDocuments message >>=  textDocumentWithUri uri


messageFromUri :: FilePath -> Uri -> IO (Maybe S.TextDocument)
messageFromUri currentDirectory uri = do
  let maybePath = uriToFilePath uri
  let relativePath = fmap (makeRelative currentDirectory) maybePath
  case relativePath of
    Just filePath -> do
      textDocumentsForUri filePath
    Nothing -> return Nothing


findSymbolOoccurence :: Position -> TextDocument -> Maybe S.SymbolOccurrence
findSymbolOoccurence position textDocument =
      findOccurrenceAtPosition position textDocument


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
