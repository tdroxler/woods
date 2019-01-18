{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main (main) where

import           Control.Concurrent                    (ThreadId, forkIO,
                                                        killThread, threadDelay)
import qualified Control.Exception                     as E
import qualified Data.Aeson                            as JSON
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

main :: IO ()
main = withSocketsDo $ do
  --TODO cleaner way to stop server loop when receiving `ExitNotification` from client?
  serverThreadId <- forkIO serverLoop
  listenClient serverThreadId
  where
    serverLoop = do
      res <- serverConnection
      case res of
        Left e  -> do
          logFile ("Exception in server connection: " ++ (show e))
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
      sock <- socket AF_UNIX Stream defaultProtocol
      maybeSbtActive <- readSbtServerUri
      case maybeSbtActive of
        Nothing -> return sock
        Just sbtActive -> do
          -- Remove `local://` in front of the uri
          let uri = drop 8 $ uriFromSbtActive sbtActive
          logFile $ "SBT server uri: " ++ uri
          res <- tryConnection sock uri
          logFile "Trying to connect to server"
          case res of
            Left e -> do
              logFile $ "Error while trying to connect to server: " ++ (show e)
              close sock
              threadDelay 1000000
              logFile $ "Retrying to connect"
              open
            Right r -> do
              logFile "Connected to server"
              return sock
    tryConnection :: Socket -> String -> IO (Either E.IOException ())
    tryConnection sock uri = E.try $ (connect sock $ SockAddrUnix uri)
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
    listenClient :: ThreadId -> IO ()
    listenClient serverThreadId = do
        loop BS.empty
        where
          loop :: BS.ByteString -> IO ()
          loop prevData = do
            newData <- BSL.hGet stdin 1
            let (contents, rest) = consumeData $ BS.append prevData (BSL.toStrict newData)
            let initializes = fromContents contents :: [InitializeRequest]
            let exits = fromContents contents :: [ExitNotification]
            let response = (map initRepsonseFromRequest initializes)
            mapM_ sendToClient response
            case exits of
              [] -> do
                return ()
              exit -> do
                logFile "<< ExitNotification from Client"
                killThread serverThreadId
                exitSuccess
            loop rest


logFile :: String -> IO ()
logFile str = BSL.appendFile "/tmp/gimmerrors.log" $ BSL.fromStrict (BS.pack $ (str ++ "\n"))


data SbtActive =  SbtActive { uri :: String } deriving (Show, Generic, JSON.FromJSON)


uriFromSbtActive :: SbtActive -> String
uriFromSbtActive sbtActive = case sbtActive of
  SbtActive uri -> uri


readSbtServerUri :: IO (Maybe SbtActive)
readSbtServerUri = do
  str <- BSL.readFile "project/target/active.json"
  return $ JSON.decode str
