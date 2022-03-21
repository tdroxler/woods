-- {-# LANGUAGE DeriveGeneric     #-}
-- module Main (main) where

-- import qualified Data.ByteString.Lazy                  as BSL
-- import           Control.Concurrent                    (forkIO, threadDelay)
-- import qualified Control.Exception                     as E
-- import           Control.Monad (forever)
-- import qualified Data.ByteString.Char8                 as BS
-- import qualified Data.Aeson                            as JSON
-- import           Network.Socket                        hiding (recv)
-- import           Network.Socket                        (Socket)
-- import           Network.Socket.ByteString             (recv)
-- import qualified Language.LSP.Types as LSP
-- import           System.Exit                           (exitSuccess)
-- import           System.IO                             (IO, hWaitForInput, stdin)
-- import           LSP
-- import           JSONRPC
-- import           JumpToDefinition
-- import           FindReferences
-- import           Rename
-- import           SarsiConnect (sarsi)
-- import           GHC.Generics


-- newtype Method = Method {
--   method :: LSP.ClientMethod
-- } deriving (Generic, Show)
-- instance JSON.FromJSON Method

-- main :: IO ()
-- main = withSocketsDo $ do
--   forkIO sarsi
--   listenClient
--   where
--     listenClient :: IO ()
--     listenClient = jsonRpcLoop (hWaitForInput stdin (-1) >> BS.hGetNonBlocking stdin 1024) () handleClientContent
--       where
--         handleClientContent content _ = do
--           case fromContent content :: Maybe Method of
--             Just (Method LSP.Initialize) ->
--               methodHandler content (return . initRepsonseFromRequest)
--             Just (Method LSP.TextDocumentReferences) ->
--               methodHandler content referenceRequestToResponse
--             Just (Method LSP.TextDocumentDefinition) ->
--               methodHandler content definitionRequestToResponse
--             Just (Method LSP.TextDocumentRename) ->
--               methodHandler content renameRequestToResponse
--             Just (Method LSP.Exit) -> exitSuccess
--             Just other -> return ()
--             Nothing -> return ()
--           where
--             methodHandler :: JSON.FromJSON a => JSON.ToJSON b => BS.ByteString ->  (a -> IO b) -> IO()
--             methodHandler content requestToResponse =
--               case fromContent content of
--                 Nothing -> return ()
--                 Just thing -> requestToResponse thing  >>= sendToClient


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T

handlers :: Handlers (LspM ())
handlers = mconcat
  [ requestHandler SInitialize $ \_not -> do
    _not
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      responder (Right $ Just rsp)
  ]

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ const $ Right ()
  , defaultConfig = ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }
