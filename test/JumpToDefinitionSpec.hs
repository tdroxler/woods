{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinitionSpec where

import Test.Hspec
import Language.Haskell.LSP.Types

import JumpToDefinition
import Fixtures

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
      definitionRequestToResponse alreadyOnDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in same file" $
      definitionRequestToResponse fooDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in different file" $
      definitionRequestToResponse fooInBarDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in other module" $
      definitionRequestToResponse moduleInFooDefinitionRequest >>= shouldBe moduleDefinitionResponse
