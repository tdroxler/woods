{-# LANGUAGE OverloadedStrings #-}

module CTags  where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Lens.Micro
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import qualified Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (DefinitionRequest, DefinitionResponse)
import System.Directory (getCurrentDirectory, findExecutable)
import System.Process (readProcess)
import Helpers

-- "This is pure Haskell code!" Obi-Wan Kenobi

ctagsLocationFromRequest :: DefinitionRequest -> String -> IO ([L.Location])
ctagsLocationFromRequest definitionRequest hint = do
  tagsLspExist <- findExecutable "tags-lsp"
  case tagsLspExist of
    Just tagsLsp -> do
      currentDirectory <- getCurrentDirectory
      res <- readProcess tagsLsp [BSLC.unpack $ JSON.encode definitionRequest, currentDirectory ++ "/tags", hint] ""
      case JSON.decode (BSLC.pack res) :: Maybe DefinitionResponse of
        Nothing ->
          return []
        Just response -> do
          case response^. LSPLens.result of
            Nothing  ->
              return []
            Just locations  -> do
              case locations of
                L.SingleLoc location -> return [location]
                L.MultiLoc locations -> return locations
    Nothing -> return []
