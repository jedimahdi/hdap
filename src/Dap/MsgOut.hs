module Dap.MsgOut where

import Dap.Env
import Dap.Session
import Dap.Types
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.TH
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.STM
import Utils
import Control.Monad (forever)

app :: Session -> IO ()
app session = forever $ do
  let msgOuts = session ^. msgOutChan
  let stream = session ^. sessionStream
  request <- atomically $ readTQueue msgOuts
  Stream.write stream (addContentLength (Aeson.encode request))
