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
import GHC.Generics (Generic)
import Network.WebSockets.Stream qualified as Stream
import Prettyprinter
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.STM
import Utils

type EventId = Int

data Event = Event
  { id :: EventId
  , event :: Text
  , body :: Maybe Value
  }
  deriving (Show, Generic)

deriving via ViaJSON Event instance Pretty Event

instance FromJSON Event where
  parseJSON = withObject "Event" $ \obj -> do
    id <- obj .: "seq"
    event <- obj .: "event"
    body <- obj .:? "body"
    pure $ Event {..}

instance ToJSON Event where
  toJSON = genericToJSON dapOptions
