{-# LANGUAGE OverloadedStrings #-}
module JSONRPC (fromContent, fromContents, sendToClient, jsonRpcLoop) where

import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Aeson                            as JSON
import           Data.Maybe                            (catMaybes)
import           System.IO                             (IO, hFlush, stdout)


jsonRpcLoop :: IO BS.ByteString -> s -> (BS.ByteString -> s -> IO s) -> IO s
jsonRpcLoop consumer state =
  ioLoop consumer state BS.empty
  where
    ioLoop consumer state acc action = do
      newData <- consumer
      if newData == BS.empty
        then return state
      else loop acc newData state action
    loop :: BS.ByteString -> BS.ByteString -> s ->  (BS.ByteString -> s -> IO s) -> IO s
    loop acc newData state action = do
      let fullData = BS.append acc newData
      let (maybeMessage, rest) = extractJsonRpcMessage fullData
      case maybeMessage of
        Just  message -> do
          newState <- action message state
          loop rest BS.empty newState action
        Nothing -> ioLoop consumer state fullData action


extractJsonRpcMessage :: BS.ByteString -> (Maybe BS.ByteString, BS.ByteString)
extractJsonRpcMessage acc =
      case readContentLength acc of
        Done rst len ->
          getMsg len rst
        _ ->
          (Nothing, acc)


readContentLength :: BS.ByteString -> IResult BS.ByteString Int
readContentLength = parse contentLengthParser


contentLengthParser :: Parser Int
contentLengthParser = do
  _ <- string "Content-Length: "
  len <- decimal
  _ <- manyTill anyChar (string _TWO_CRLF)
  return len


getMsg :: Int -> BS.ByteString -> (Maybe BS.ByteString, BS.ByteString)
getMsg len msg =
  if BS.length msg < len
    then (Nothing, msg)
    else case BS.splitAt len msg of
      (content, next) -> (Just content, next)

_TWO_CRLF = BS.pack "\r\n\r\n"


fromContents :: JSON.FromJSON a => [BS.ByteString] -> [a]
fromContents contents = do
  let maybes = map (JSON.decode . BSL.fromStrict) contents
  catMaybes maybes


fromContent :: JSON.FromJSON a => BS.ByteString -> Maybe a
fromContent content =
  JSON.decode (BSL.fromStrict content)


sendToClient :: JSON.ToJSON a => a -> IO ()
sendToClient message = do
    let str = JSON.encode message
    let out = BSL.concat
                 [ stringToBLS $ "Content-Length: " ++ show (BSL.length str)
                 , BSL.fromStrict _TWO_CRLF
                 , str ]
    BSL.hPut stdout out
    hFlush stdout


stringToBLS :: String -> BSL.ByteString
stringToBLS = BSL.fromStrict . BS.pack
