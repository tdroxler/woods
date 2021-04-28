{-# LANGUAGE OverloadedStrings #-}
module SarsiConnect (sarsi) where

import System.IO (IO)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (getCurrentDirectory)
import Codec.Sarsi (Event (..), Level (..), Location (..), Message (..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar', newTVar, stateTVar )
import Data.Machine (ProcessT, asParts, final, runT, scan, sinkPart_, (<~))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Sarsi (Topic (..), getBroker, getTopic)
import qualified Sarsi as Sarsi
import Sarsi.Consumer (consumeOrWait)
import System.IO.Machine (sinkIO)
import System.IO.Unsafe (unsafePerformIO)
import qualified Language.Haskell.LSP.Types as LSP

import JSONRPC (sendToClient)


type State = Map LSP.Uri [LSP.Diagnostic]

data Action = Clean | Pub (LSP.Uri, LSP.Diagnostic)

currentDirectory = ((unsafePerformIO getCurrentDirectory) ++ "/")

sarsi :: IO ()
sarsi = do
  b <- getBroker
  t <- getTopic b "."
  var <- atomically $ newTVar Map.empty
  consumeOrWait t $ consumer t var
  where
    consumer (Topic _ tp) var first src = do
      res <- runT $ final <~ sinkPart_ id ((sinkIO $ sender tp var) <~ asParts) <~ converter (maybe True id first) <~ src
      return $ Left $ head res


convert :: Bool -> Event -> (Bool, [Action])
convert _ (Start _) = (False, [Clean])
convert _ (Finish e w )=  (True, [])
convert first (Notify msg) = (False, (if first then [Clean] else []) ++ [Pub $ msgTo msg])
  where
    msgTo msg@(Message (Location fp _ _) _ _) =
      (LSP.filePathToUri $ (currentDirectory ++ Text.unpack fp), msgToDiagnostic msg)


converter :: Bool -> ProcessT IO Event (Bool, [Action])
converter first = scan f (first, []) where f (first', _) event = convert first' event


sender :: FilePath -> TVar State -> Action -> IO ()
sender tp var action = do
   diagsNotif <- case action of
     Clean -> cleanState var
     Pub (uri, diags) -> updateState var (uri,diags)
   mapM_ sendToClient diagsNotif


updateState :: TVar State -> (LSP.Uri, LSP.Diagnostic) -> IO [LSP.PublishDiagnosticsNotification]
updateState var (uri, diag) = do
  atomically $ modifyTVar' var (\s -> Map.insert uri (diags uri s) s)
  newState <- (atomically $ readTVar var)
  return $ notifsFromState newState
  where
  diags uri state = case (Map.lookup uri state) of
     Nothing -> [diag]
     Just ds -> ds ++ [diag]


cleanState :: TVar State -> IO [LSP.PublishDiagnosticsNotification]
cleanState var  = do
  state <- atomically $ stateTVar var (\s -> (Map.map (\_ -> []) s, Map.empty))
  return $ notifsFromState state


notifsFromState :: State -> [LSP.PublishDiagnosticsNotification]
notifsFromState state = fmap notif (Map.toList state)
  where
  notif (uri, diags) =
    LSP.NotificationMessage
      "2.0"
      LSP.TextDocumentPublishDiagnostics
      (LSP.PublishDiagnosticsParams uri (LSP.List $ diags))


msgToDiagnostic :: Message -> LSP.Diagnostic
msgToDiagnostic (Message (Location fp col ln) lvl txts) =
  LSP.Diagnostic
    (LSP.Range (LSP.Position (ln-1) (col-1)) (LSP.Position (ln-1) (col)))
    (Just $ levelToSeverity lvl)
    Nothing
    Nothing
    (Text.concat txts)
    Nothing
    Nothing
  where
    levelToSeverity Warning = LSP.DsWarning
    levelToSeverity Error = LSP.DsError
