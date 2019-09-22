{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main (main) where

import           Control.Concurrent                    (ThreadId, forkIO,
                                                        killThread, threadDelay)
import qualified Control.Exception                     as E
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
import           System.Exit                           (exitSuccess)
import           System.IO
import           LSP
import           JSONRPC
import           Sbt

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
    listenClient = do
        loop BS.empty
        where
          loop :: BS.ByteString -> IO ()
          loop prevData = do
            hWaitForInput stdin (-1)
            newData <- BS.hGetNonBlocking stdin 2048
            let (contents, rest) = consumeData $ BS.append prevData newData
            let initializes = fromContents contents :: [InitializeRequest]
            let exits = fromContents contents :: [ExitNotification]
            let response = (map initRepsonseFromRequest initializes)
            mapM_ sendToClient response
            case exits of
              [] -> do
                return ()
              exit -> do
                logFile "<< ExitNotification from Client"
                exitSuccess
            loop rest


logFile :: String -> IO ()
logFile str = BSL.appendFile "/tmp/gimmerrors.log" $ BSL.fromStrict (BS.pack $ (str ++ "\n"))
