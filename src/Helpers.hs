module Helpers where

import Data.Text as T
import Language.Haskell.LSP.Types as L
import Data.Int
import Lens.Micro
import Data.List as List
import Data.ProtoLens (decodeMessage)
import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (documents, occurrences, role, symbol, startLine, endLine, startCharacter, endCharacter, range, uri)
import Lens.Micro.Extras (view)
import System.FilePath.Find as Find
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (makeRelative)
import qualified Data.ByteString.Char8 as BS

occurrenceAtPosition :: Position -> S.TextDocument -> Maybe S.SymbolOccurrence
occurrenceAtPosition position textDocument = List.find (\symbol -> isPosititionInRange position (symbol^.range)) (textDocument^.occurrences)

isPosititionInRange :: Position -> S.Range -> Bool
isPosititionInRange position range =
     _line position >= int32ToInt  (range^.startLine)
  && _line position <= int32ToInt  (range^.endLine)
  && _character position >= int32ToInt  (range^.startCharacter)
  && _character position <= int32ToInt  (range^.endCharacter)


int32ToInt :: Int32 -> Int
int32ToInt int32 = fromIntegral int32 :: Int

listAllfiles :: IO [FilePath]
listAllfiles = getCurrentDirectory >>= Find.find always (extension ==? ".semanticdb")


locationFromSymbolWithTextDocument :: (S.SymbolOccurrence, S.TextDocument) -> IO L.Location
locationFromSymbolWithTextDocument symbolWithTextDocument = do
  let definitionSymbol = fst symbolWithTextDocument
  let definitionTexDocument = snd symbolWithTextDocument
  definitionUri <- uriFromTextDocument definitionTexDocument
  return $ lspLocation definitionUri definitionSymbol


uriFromTextDocument :: S.TextDocument -> IO Uri
uriFromTextDocument textDocument = do
  currentDirectory  <- getCurrentDirectory
  return $ Uri (T.pack( "file://" ++ currentDirectory ++ "/" ++ T.unpack (textDocument^.uri)))


lspLocation :: Uri -> S.SymbolOccurrence -> L.Location
lspLocation uri symbolOccurence =
  L.Location
    uri
    (semanticdbRangeToLSPRange (symbolOccurence^.range))


semanticdbRangeToLSPRange :: S.Range -> L.Range
semanticdbRangeToLSPRange sRange =
  let
    startPosition = L.Position (int32ToInt (sRange^.startLine)) (int32ToInt (sRange^.startCharacter))
    endPosition = L.Position (int32ToInt (sRange^.endLine)) (int32ToInt (sRange^.endCharacter))
  in Range startPosition endPosition


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

textDocumentFromUriAndSemanticdbFile :: FilePath -> FilePath -> IO(Maybe S.TextDocument)
textDocumentFromUriAndSemanticdbFile uri semanticdbFile = do
  maybeTextDocuments <- decodeTextDocuments  <$> BS.readFile semanticdbFile
  return (maybeTextDocuments >>= textDocumentWithUri uri)

decodeTextDocuments :: BS.ByteString -> Maybe S.TextDocuments
decodeTextDocuments message =
  case decodeMessage message of
    Left e -> Nothing
    Right msg -> Just msg


textDocumentWithUri :: FilePath -> S.TextDocuments -> Maybe S.TextDocument
textDocumentWithUri uri textDocuments = List.find (isSameUri uri) $ textDocuments ^.documents

isSameUri :: FilePath -> S.TextDocument -> Bool
isSameUri filePath textDocument =  T.unpack (textDocument^.uri) == filePath


listTextDocumentFromFilePath :: FilePath -> IO [S.TextDocument]
listTextDocumentFromFilePath filePath = do
  message <- BS.readFile filePath
  return $ case decodeTextDocuments message of
    Nothing -> []
    Just textDocuments ->  textDocuments ^.documents
