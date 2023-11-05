module Dap.MsgIn where

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

app :: Session -> IO ()
app session = do
  let stream = session ^. sessionStream
  readLoop stream handleBody (pure ())
  where
    handleBody :: Value -> IO ()
    handleBody value = do
      -- printJSON value
      let responsesChan = session ^. msgInChan
      let mr = parseMaybe (parseJSON @MsgIn) value
      case mr of
        Just response -> do
          -- print response
          atomically $ writeTQueue responsesChan response
        Nothing -> do
          pure ()
