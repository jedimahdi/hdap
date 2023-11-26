module Dap.Response where

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

type ResponseId = Int

data Response = Response
  { id :: ResponseId
  , requestId :: Int
  , success :: Bool
  , command :: Text
  , message :: Maybe Text
  , body :: Maybe Value
  }
  deriving (Show, Generic)

deriving via ViaJSON Response instance Pretty Response

instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    id <- obj .: "seq"
    success <- obj .: "success"
    requestId <- obj .: "request_seq"
    command <- obj .: "command"
    message <- obj .:? "message"
    body <- obj .:? "body"
    pure $ Response {..}

instance ToJSON Response where
  toJSON = genericToJSON dapOptions
