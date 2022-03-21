{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinitionSpec where

import Test.Hspec
import Language.LSP.Types

import JumpToDefinition
import Fixtures

mkRequest identifier position =
  RequestMessage
    "2.0"
    messageId
    TextDocumentDefinition $ TextDocumentPositionParams identifier position Nothing


fooDefinitionRequest :: DefinitionRequest
fooDefinitionRequest = mkRequest fooTextDocumentIdentifier printFooPosition


fooInBarDefinitionRequest :: DefinitionRequest
fooInBarDefinitionRequest = mkRequest barTextDocumentIdentifier fooInBarPosition


alreadyOnDefinitionRequest :: DefinitionRequest
alreadyOnDefinitionRequest = mkRequest fooTextDocumentIdentifier fooPosition

moduleInFooDefinitionRequest :: DefinitionRequest
moduleInFooDefinitionRequest = mkRequest fooTextDocumentIdentifier moduleInFooPosition

stringDependencyRequest :: DefinitionRequest
stringDependencyRequest = mkRequest fooTextDocumentIdentifier stringPosition

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


stringDefinitionResponse :: DefinitionResponse
stringDefinitionResponse = mkResponse stringLocation


spec :: Spec
spec =
  describe "JumpToDefinition" $ do
    it "find the correct definition when cursor is already on the definition" $
      definitionRequestToResponse alreadyOnDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in same file" $
      definitionRequestToResponse fooDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in different file" $
      definitionRequestToResponse fooInBarDefinitionRequest >>= shouldBe fooDefinitionResponse
    it "find the correct definition when in other module" $
      definitionRequestToResponse moduleInFooDefinitionRequest >>= shouldBe moduleDefinitionResponse
    it "find dependencies definition with tags file" $
      definitionRequestToResponse stringDependencyRequest >>= shouldBe stringDefinitionResponse
