{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main (main) where

import           Control.Concurrent                    (ThreadId, forkIO,
                                                        killThread, threadDelay)
import qualified Control.Exception                     as E
import           Control.Monad (forever)
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.List                             (drop, map)
import           GHC.Generics                          (Generic)
import           Network.Socket                        hiding (recv)
import           Network.Socket                        (Family (AF_UNIX),
                                                        SockAddr (SockAddrUnix),
                                                        Socket,
                                                        SocketType (Stream),
                                                        defaultProtocol, socket)
import           Network.Socket.ByteString             (recv)
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens
import           Lens.Micro
import           System.Exit                           (exitSuccess)
import           System.IO
import           LSP
import           JSONRPC
import           Sbt
import           JumpToDefinition

main :: IO ()
main = withSocketsDo $ do
  forkIO serverLoop
  listenClient
  where
    serverLoop = do
      res <- serverConnection
      case res of
        Left e  -> do
          logFile ("Exception in server connection: " ++ (show e) ++ ". Retrying to connect")
          threadDelay 1000000
          serverLoop
          --Trying to reconnect anyway
        Right a -> return a
    serverConnection :: IO (Either E.IOException ())
    serverConnection = E.try $ E.bracket open closing talk
    closing :: Socket -> IO ()
    closing sock = do
      logFile "Closing server socket"
      close sock
    open :: IO Socket
    open = do
      maybeSocket <- connectToSbtServer
      case maybeSocket of
        Nothing -> do
          threadDelay 1000000
          logFile $ "Retrying to connect"
          open
        Just sock -> do
          logFile $ "Connected to sbt"
          return sock
    talk :: Socket -> IO ()
    talk sock = do
      res <- loop [] sock BS.empty
      case res of
        Just a -> return ()
        Nothing -> do
          logFile "Disconnected from server"
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
      case (fromContent content :: Maybe DefinitionRequest) of
        Nothing -> return ()
        Just definitionRequest ->
          definitionRequestToResponse definitionRequest >>= sendToClient
      case (fromContent content :: Maybe InitializeRequest) of
        Nothing -> return ()
        Just initialize -> do
          let response =  initRepsonseFromRequest initialize
          sendToClient response
      case (fromContent content :: Maybe ExitNotification) of
        Nothing -> return ()
        Just exit -> do
          logFile "<< ExitNotification from Client"
          exitSuccess


getContentLength :: IO(Int)
getContentLength = do
  sizeBS <- loop BS.empty
  return $ read (BS.unpack sizeBS)
    where
      loop :: BS.ByteString -> IO(BS.ByteString)
      loop acc = do
         char <- BS.hGet stdin 1
         if  char == BS.pack "\r"
            then do
              rest <- BS.hGet stdin 3
              return acc
         else loop $ BS.append acc char

logFile :: String -> IO ()
logFile str = BSL.appendFile "/tmp/gimmerrors.log" $ BSL.fromStrict (BS.pack $ (str ++ "\n"))
