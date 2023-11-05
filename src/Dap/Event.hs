{-# LANGUAGE TemplateHaskell #-}

module Dap.Event where

import Dap.Protocol
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

type EventId = Int

data Event = Event
  { _eventSeq :: EventId
  , _eventEvent :: Text
  , _eventBody :: Maybe Value
  }
  deriving (Show)
makeLenses ''Event

instance FromJSON Event where
  parseJSON = withObject "Event" $ \obj -> do
    _eventSeq <- obj .: "seq"
    _eventEvent <- obj .: "event"
    _eventBody <- obj .:? "body"
    pure $ Event {..}
