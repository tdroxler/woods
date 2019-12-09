{-# LANGUAGE DeriveGeneric     #-}
module Main (main) where

import qualified Data.ByteString.Lazy                  as BSL
import           Control.Concurrent                    (forkIO, threadDelay)
import qualified Control.Exception                     as E
import           Control.Monad (forever)
import qualified Data.ByteString.Char8                 as BS
import qualified Data.Aeson                            as JSON
import           Network.Socket                        hiding (recv)
import           Network.Socket                        (Socket)
import           Network.Socket.ByteString             (recv)
import qualified Language.Haskell.LSP.Types as LSP
import           System.Exit                           (exitSuccess)
import           System.IO                             (IO, hWaitForInput, stdin)
import           LSP
import           JSONRPC
import           Sbt
import           JumpToDefinition
import           FindReferences
import           GHC.Generics

newtype Method = Method {
  method :: LSP.ClientMethod
} deriving (Generic, Show)
instance JSON.FromJSON Method

main :: IO ()
main = withSocketsDo $ do
  forkIO serverLoop
  listenClient
  where
    serverLoop = do
      res <- serverConnection
      case res of
        Left e  -> do
          threadDelay 1000000
          serverLoop
          --Trying to reconnect anyway
        Right a -> return a
    serverConnection :: IO (Either E.IOException ())
    serverConnection = E.try $ E.bracket open close talk
    open :: IO Socket
    open = do
      maybeSocket <- connectToSbtServer
      case maybeSocket of
        Nothing -> do
          threadDelay 1000000
          open
        Just sock ->
          return sock
    talk :: Socket -> IO ()
    talk sock =
      jsonRpcLoop (recv sock 1024) [] handleContent >> serverLoop
    handleContent content diags = do
      let publishDiagnostics = fromContents [content] :: [LSP.PublishDiagnosticsNotification]
      let (toSend, nextDiags) =  diagnosticsLoop diags publishDiagnostics
      mapM_ sendToClient toSend >> return nextDiags
    listenClient :: IO ()
    listenClient = jsonRpcLoop (hWaitForInput stdin (-1) >> BS.hGetNonBlocking stdin 1024) () handleClientContent
      where
        handleClientContent content _ =
          case fromContent content :: Maybe Method of
            Just (Method LSP.Initialize) ->
              methodHandler content (return . initRepsonseFromRequest)
            Just (Method LSP.TextDocumentReferences) ->
              methodHandler content referenceRequestToResponse
            Just (Method LSP.TextDocumentDefinition) ->
              methodHandler content definitionRequestToResponse
            Just (Method LSP.Exit) -> exitSuccess
            Just other -> return ()
            Nothing -> return ()
          where
            methodHandler :: JSON.FromJSON a => JSON.ToJSON b => BS.ByteString ->  (a -> IO b) -> IO()
            methodHandler content requestToResponse =
              case fromContent content of
                Nothing -> return ()
                Just thing -> requestToResponse thing  >>= sendToClient
