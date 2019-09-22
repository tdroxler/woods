{-# LANGUAGE OverloadedStrings #-}
module JSONRPC (consumeData, fromContent, fromContents, sendToClient) where

import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Aeson                            as JSON
import           Data.Maybe                            (maybeToList)
import           System.IO

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

_TWO_CRLF = BS.pack "\r\n\r\n"


fromContents :: JSON.FromJSON a => [BS.ByteString] -> [a]
fromContents contents = do
  let maybes = map (\b -> JSON.decode (BSL.fromStrict b)) contents
  concat $ map (\m -> maybeToList m) maybes

fromContent :: JSON.FromJSON a => BS.ByteString -> Maybe a
fromContent content = do
  JSON.decode (BSL.fromStrict content)

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


stringToBLS :: String -> BSL.ByteString
stringToBLS = BSL.fromStrict . BS.pack
