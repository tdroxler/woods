{-# LANGUAGE OverloadedStrings #-}

module FindReferencesSpec where

import Test.Hspec
import Language.Haskell.LSP.Types

import System.Directory (setCurrentDirectory)

import Fixtures
import FindReferences


mkRequest identifier position =
  RequestMessage
    "2.0"
    messageId
    TextDocumentReferences $ ReferenceParams identifier position (ReferenceContext False)


fooReferencesRequest :: ReferencesRequest
fooReferencesRequest = mkRequest fooTextDocumentIdentifier printFooPosition


mkResponse locations =
    ResponseMessage
      "2.0"
      (responseId messageId)
      (Just $ List locations)
      Nothing


fooReferencesResponse :: ReferencesResponse
fooReferencesResponse = mkResponse [fooInBarLocation, fooLocation, printFooLocation]


spec :: Spec
spec =
  describe "FindReferences" $
    it "find all references correctly" $ do
      setCurrentDirectory "test/resources"
      referenceRequestToResponse fooReferencesRequest >>= shouldBe fooReferencesResponse
