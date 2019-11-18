{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Sbt (connectToSbtServer) where

import qualified Control.Exception                     as E
import qualified Data.ByteString.Lazy                  as BSL
import           GHC.Generics                          (Generic)
import qualified Data.Aeson                            as JSON
import           System.IO
import           Network.Socket                        hiding (recv)


newtype SbtActive =  SbtActive { uri :: String } deriving (Show, Generic, JSON.FromJSON)

connectToSbtServer :: IO (Maybe Socket)
connectToSbtServer = do
  sock <- socket AF_UNIX Stream defaultProtocol
  maybeSbtActive <- readSbtServerUri
  case maybeSbtActive of
    Nothing -> return $ Just sock
    Just sbtActive -> do
      -- Remove `local://` in front of the uri
      let uri = drop 8 $ uriFromSbtActive sbtActive
      res <- tryConnection sock uri
      case res of
        Left e -> do
          close sock
          return Nothing
        Right r ->
          return $ Just sock


tryConnection :: Socket -> String -> IO (Either E.IOException ())
tryConnection sock uri = E.try (connect sock $ SockAddrUnix uri)

uriFromSbtActive :: SbtActive -> String
uriFromSbtActive sbtActive = case sbtActive of
  SbtActive uri -> uri


readSbtServerUri :: IO (Maybe SbtActive)
readSbtServerUri = do
  str <- BSL.readFile "project/target/active.json"
  return $ JSON.decode str
