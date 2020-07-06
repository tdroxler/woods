{-# LANGUAGE TupleSections #-}

module Rename (renameRequestToResponse)  where

import Data.Traversable (traverse)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import qualified Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (RenameRequest, RenameResponse, )
import Data.List as List
import Data.Text as T
import System.Process (callCommand)

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (occurrences, role, symbol)
import LSP (renameResponse)
import FindReferences (findLocations)

renameRequestToResponse ::  RenameRequest -> IO RenameResponse
renameRequestToResponse renameRequest = do
  res <- renameFromRequest renameRequest
  return $ renameResponse renameRequest


--TODO renameFromRequest :: RenameRequest -> IO WorkspaceEdit
renameFromRequest :: RenameRequest -> IO ()
renameFromRequest renameRequest = do
  let pos = renameRequest^.(LSPLens.params . LSPLens.position)
  let uri = renameRequest^.(LSPLens.params . LSPLens.textDocument . LSPLens.uri)
  let newText = renameRequest^.(LSPLens.params . LSPLens.newName)
  locations <- findLocations uri pos
  traverse (rename newText) locations
  return ()


--TODO rename :: T.Text -> L.Location -> IO (Maybe (Uri, TextEdit))
rename :: T.Text -> L.Location -> IO ()
rename new location =
  case (L.uriToFilePath $ location^.(LSPLens.uri)) of
    Just uri -> do
      let line = location^.(LSPLens.range . LSPLens.start . LSPLens.line) + 1
      let startChar = location^.(LSPLens.range . LSPLens.start . LSPLens.character)
      let endChar = location^.(LSPLens.range . LSPLens.end . LSPLens.character)
      let worldLength = endChar - startChar
      let regex =  (show line) ++ "'s/^(.{" ++ (show startChar) ++ "}).{" ++ (show worldLength) ++ "}/\\1" ++ (T.unpack new) ++ "/'"
      let command = "sed -Ei " ++ regex ++ " " ++ uri
      callCommand  command
    Nothing -> return ()
