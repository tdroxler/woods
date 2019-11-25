{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import           Control.Concurrent                    (forkIO, threadDelay)
import qualified Control.Exception                     as E
import           Control.Monad (forever)
import qualified Data.ByteString.Char8                 as BS
import qualified Data.Aeson                            as JSON
import           Network.Socket                        hiding (recv)
import           Network.Socket                        (Socket)
import           Network.Socket.ByteString             (recv)
import           Language.Haskell.LSP.Types
import           System.Exit                           (exitSuccess)
import           System.IO
import           LSP
import           JSONRPC
import           Sbt
import           JumpToDefinition
import           FindReferences
import           Hover
import           GHC.Generics

newtype Method = Method {
  method :: ClientMethod
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
    talk sock = do
      res <- loop [] sock BS.empty
      case res of
        Just a -> return ()
        Nothing ->
          serverLoop
    loop :: [Uri] -> Socket -> BS.ByteString -> IO (Maybe ())
    loop diags sock prevData = do
      newData <- recv sock 1024
      if newData == BS.empty
        then return Nothing
      else do
        let (contents, rest) = consumeData $ BS.append prevData newData
        let publishDiagnostics = fromContents contents :: [PublishDiagnosticsNotification]
        let (toSend, nextDiags) =  diagnosticsLoop diags publishDiagnostics
        mapM_ sendToClient toSend
        loop nextDiags sock rest
    listenClient :: IO ()
    listenClient = forever $ do
      hWaitForInput stdin (-1)
      BS.hGetNonBlocking stdin 15 -- "Content-Length:"
      size <- getContentLength
      content <- BS.hGet stdin size
      case fromContent content :: Maybe Method of
        Just (Method Initialize) ->
          methodHandler content (return . initRepsonseFromRequest)
        Just (Method TextDocumentReferences) ->
          methodHandler content referenceRequestToResponse
        Just (Method TextDocumentDefinition) ->
          methodHandler content definitionRequestToResponse
        Just (Method TextDocumentHover) ->
          methodHandler content hoverRequestToResponse
        Just (Method Exit) -> exitSuccess
        Just other -> return ()
        Nothing -> return ()
      where
        methodHandler :: JSON.FromJSON a => JSON.ToJSON b => BS.ByteString ->  (a -> IO b) -> IO()
        methodHandler content requestToResponse =
          case fromContent content of
            Nothing -> return ()
            Just thing ->
              requestToResponse thing  >>= sendToClient


getContentLength :: IO Int
getContentLength = do
  sizeBS <- loop BS.empty
  return $ read (BS.unpack sizeBS)
    where
      loop :: BS.ByteString -> IO BS.ByteString
      loop acc = do
         char <- BS.hGet stdin 1
         if  char == BS.pack "\r"
            then do
              rest <- BS.hGet stdin 3
              return acc
         else loop $ BS.append acc char
