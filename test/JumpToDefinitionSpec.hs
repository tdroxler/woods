{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinitionSpec where

import Test.Hspec
import Language.Haskell.LSP.Types

import JumpToDefinition

fooUri = Uri  "file://test/resources/src/main/scala/Foo.scala"

fooRange = Range (Position 2 6) (Position 2 9)

fooLocation = Location fooUri fooRange

printFooPosition = Position 5 15

fooTextDocumentIdentifier = TextDocumentIdentifier fooUri

messageId = IdInt 1

fooDefinitionRequest :: DefinitionRequest
fooDefinitionRequest =
  RequestMessage
    "2.0"
    messageId
    TextDocumentDefinition
    (TextDocumentPositionParams
      fooTextDocumentIdentifier
      printFooPosition
    )


fooDefinitionResponse :: DefinitionResponse
fooDefinitionResponse =
    ResponseMessage
      "2.0"
      (responseId messageId)
      (Just $ SingleLoc fooLocation)
      Nothing


spec :: Spec
spec = do
  describe "JumpToDefinition" $ do
    it "find the correct definition" $ do
      response <- definitionRequestToResponse fooDefinitionRequest
      response `shouldBe` fooDefinitionResponse
