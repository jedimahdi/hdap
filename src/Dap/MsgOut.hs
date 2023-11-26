module Dap.MsgOut where

import Control.Monad (forever)
import Dap.Env
import Dap.Session
import Dap.Types
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.STM
import Utils

app :: Session -> IO ()
app session = forever $ do
  -- request <- atomically $ readTMVar session.requestToSend
  -- request <- atomically $ readTQueue msgOuts
  stream <- readIORef session.streamRef
  request <- atomically $ readTChan session.clientOut
  Stream.write stream (addContentLength (Aeson.encode request))

-- atomically $ takeTMVar session.requestToSend
