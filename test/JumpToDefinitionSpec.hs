{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinitionSpec where

import Test.Hspec
import Language.Haskell.LSP.Types
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Char8 as BS

import JumpToDefinition

import Data.Text as T
import Data.List as List
import Data.ProtoLens (decodeMessage)
import Lens.Micro
import System.Directory (setCurrentDirectory)
import Proto.Semanticdb_Fields (uri)
import Proto.Semanticdb as S

currentDirectory = "file://" ++ (unsafePerformIO getCurrentDirectory)

fooPath =  currentDirectory ++ "/src/main/scala/Foo.scala"

barPath =  currentDirectory ++ "/src/main/scala/Bar.scala"

modulePath =  currentDirectory ++ "/module/src/main/scala/Module.scala"

fooUri = Uri $ T.pack fooPath

barUri = Uri $ T.pack barPath

moduleUri = Uri $ T.pack modulePath

fooRange = Range (Position 2 6) (Position 2 9)

fooLocation = Location fooUri fooRange

fooPosition = Position 2 6

printFooPosition = Position 5 15

fooInBarPosition = Position 2 14

fooTextDocumentIdentifier = TextDocumentIdentifier fooUri

barTextDocumentIdentifier = TextDocumentIdentifier barUri

moduleInFooPosition = Position 7 26

moduleRange = Range (Position 1 6) (Position 1 12)

moduleLocation = Location moduleUri moduleRange

messageId = IdInt 1

mkRequest identifier position =
  RequestMessage
    "2.0"
    messageId
    TextDocumentDefinition $ TextDocumentPositionParams identifier position


fooDefinitionRequest :: DefinitionRequest
fooDefinitionRequest = mkRequest fooTextDocumentIdentifier printFooPosition


fooInBarDefinitionRequest :: DefinitionRequest
fooInBarDefinitionRequest = mkRequest barTextDocumentIdentifier fooInBarPosition


alreadyOnDefinitionRequest :: DefinitionRequest
alreadyOnDefinitionRequest = mkRequest fooTextDocumentIdentifier fooPosition

moduleInFooDefinitionRequest :: DefinitionRequest
moduleInFooDefinitionRequest = mkRequest fooTextDocumentIdentifier moduleInFooPosition

mkResponse location =
    ResponseMessage
      "2.0"
      (responseId messageId)
      (Just $ SingleLoc location)
      Nothing

fooDefinitionResponse :: DefinitionResponse
fooDefinitionResponse = mkResponse fooLocation

moduleDefinitionResponse :: DefinitionResponse
moduleDefinitionResponse = mkResponse moduleLocation


spec :: Spec
spec = do
  describe "JumpToDefinition" $ do
    it "find the correct definition when cursor is already on the definition" $ do
      setCurrentDirectory "test/resources"
      definitionRequestToResponse alreadyOnDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in same file" $
      definitionRequestToResponse fooDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in different file" $
      definitionRequestToResponse fooInBarDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in other module" $
      definitionRequestToResponse moduleInFooDefinitionRequest >>= shouldBe moduleDefinitionResponse
