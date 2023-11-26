module Dap.MsgIn where

import Colog.Core
import Control.Monad (when)
import Dap.Env
import Dap.Event
import Dap.Protocol
import Dap.Request
import Dap.Response
import Dap.Session
import Dap.Types
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Parser qualified as Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.IORef (readIORef)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.STM
import Utils

app :: Session -> IO ()
app session = do
  readLoop (readIORef session.streamRef) handleBody (pure ())
  where
    handleBody :: Value -> IO ()
    handleBody value = do
      -- printJSON value
      let mr = parseMaybe (parseJSON @MsgIn) value
      case mr of
        Just response -> do
          -- print response
          case response of
            EventMsg event -> do
              when (event.event /= "output" && event.event /= "loadedSource") $ do
                session.logger <& ReceiveMsg response `WithSeverity` Debug
              atomically $ writeTChan session.events event
            ResponseMsg responseMsg -> do
              session.logger <& ReceiveMsg response `WithSeverity` Debug
              atomically $ modifyTVar' session.responseMap (Map.insert responseMsg.requestId responseMsg)
            RequestMsg reverseRequest -> do
              session.logger <& ReceiveMsg response `WithSeverity` Debug
              atomically $ writeTChan session.reverseRequests reverseRequest
        -- atomically $ writeTQueue responsesChan response
        Nothing -> do
          pure ()
