{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent                    (ThreadId, forkIO,
                                                        killThread, threadDelay)
import qualified Control.Exception                     as E
import qualified Data.Aeson                            as JSON
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.List                             (drop, filter, map)
import           Data.Maybe                            (maybeToList)
import           GHC.Generics                          (Generic)
import           Language.Haskell.LSP.TH.DataTypesJSON
import           Network.Socket                        hiding (recv)
import           Network.Socket                        (Family (AF_UNIX),
                                                        SockAddr (SockAddrUnix),
                                                        Socket,
                                                        SocketType (Stream),
                                                        defaultProtocol, socket)
import           Network.Socket.ByteString             (recv, sendAll)
import           System.Exit                           (exitSuccess)
import           System.IO


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
        let diagWithError = filter diagnosticErrorExist publishDiagnostics
        let diagWithErrorUri = map uriFromPublishDiagnosticsNotification diagWithError
        let cleanedDiagnostic = filter (\e -> elem (uriFromPublishDiagnosticsNotification e) diags && notElem (uriFromPublishDiagnosticsNotification e) diagWithErrorUri) publishDiagnostics
        let notCleanedYet = filter (\e -> notElem e (map uriFromPublishDiagnosticsNotification cleanedDiagnostic)) diags
        mapM_ sendToClient (cleanedDiagnostic ++ diagWithError)
        loop (notCleanedYet ++ diagWithErrorUri) sock rest


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

consumeData :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
consumeData msg =
    consume [] msg
  where
    consume :: [BS.ByteString] -> BS.ByteString -> ([BS.ByteString], BS.ByteString)
    consume acc msg =
      case readContentLength msg of
        Done rst len ->
          let (maybeContent, rest) = getMsg len rst
          in case maybeContent of
            (Just content) ->
              consume (acc ++ [content]) rest
            (Nothing) ->
              (acc, msg)
        _ ->
          (acc, msg)


readContentLength :: BS.ByteString -> IResult BS.ByteString Int
readContentLength = parse contentLengthParser


contentLengthParser :: Parser Int
contentLengthParser = do
  _ <- string "Content-Length: "
  len <- takeTill (\c ->c == '\r')
  _ <- manyTill anyChar (string _TWO_CRLF)
  return $ (read (BS.unpack len) :: Int)


getMsg :: Int -> BS.ByteString -> (Maybe BS.ByteString, BS.ByteString)
getMsg len msg =
  if BS.length msg < len
    then (Nothing, msg)
    else case BS.splitAt len msg of
      (content, next) -> (Just content, next)



fromContents :: JSON.FromJSON a => [BS.ByteString] -> [a]
fromContents contents = do
  let maybes = map (\b -> JSON.decode (BSL.fromStrict b)) contents
  concat $ map (\m -> maybeToList m) maybes

sendToClient :: JSON.ToJSON a => a -> IO ()
sendToClient message =  do
    let str = JSON.encode message
    let out = BSL.concat
                 [ stringToBLS $ "Content-Length: " ++ show (BSL.length str)
                 , BSL.fromStrict _TWO_CRLF
                 , str ]
    BSL.hPut stdout out
    hFlush stdout
    where


diagnosticErrorExist :: PublishDiagnosticsNotification -> Bool
diagnosticErrorExist = (\d -> case d of NotificationMessage _ _ params -> case params of PublishDiagnosticsParams _ diagnostics -> not (null diagnostics))


uriFromPublishDiagnosticsNotification :: PublishDiagnosticsNotification -> Uri
uriFromPublishDiagnosticsNotification notification = case notification of NotificationMessage _ _ param -> uriFromPublishDiagnosticsParams param


uriFromPublishDiagnosticsParams :: PublishDiagnosticsParams -> Uri
uriFromPublishDiagnosticsParams param = case param of PublishDiagnosticsParams uri _ -> uri


initRepsonseFromRequest :: InitializeRequest -> InitializeResponse
initRepsonseFromRequest request = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $ InitializeResponseCapabilities serverCapabilities)
      Nothing


logFile :: String -> IO ()
logFile str = BSL.appendFile "/tmp/gimmerrors.log" $ BSL.fromStrict (BS.pack $ (str ++ "\n"))


stringToBLS :: String -> BSL.ByteString
stringToBLS = BSL.fromStrict . BS.pack


-- No serverCapabilities at all for now
serverCapabilities =
  InitializeResponseCapabilitiesInner
    { _textDocumentSync                 = Nothing
    , _hoverProvider                    = Just False
    , _completionProvider               = Nothing
    , _signatureHelpProvider            = Nothing
    , _definitionProvider               = Just False
    , _referencesProvider               = Just False
    , _documentHighlightProvider        = Just False
    , _documentSymbolProvider           = Just False
    , _workspaceSymbolProvider          = Just False
    , _codeActionProvider               = Just False
    , _codeLensProvider                 = Nothing
    , _documentFormattingProvider       = Just False
    , _documentRangeFormattingProvider  = Just False
    , _documentOnTypeFormattingProvider = Nothing
    , _renameProvider                   = Just False
    , _documentLinkProvider             = Nothing
    , _executeCommandProvider           = Nothing
    , _experimental                     = Nothing
    }


_TWO_CRLF = BS.pack "\r\n\r\n"


data SbtActive =  SbtActive { uri :: String } deriving (Show, Generic, JSON.FromJSON)


uriFromSbtActive :: SbtActive -> String
uriFromSbtActive sbtActive = case sbtActive of
  SbtActive uri -> uri


readSbtServerUri :: IO (Maybe SbtActive)
readSbtServerUri = do
  str <- BSL.readFile "project/target/active.json"
  return $ JSON.decode str
