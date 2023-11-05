{-# LANGUAGE TemplateHaskell #-}

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
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.TH
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.STM
import Utils

type ResponseId = Int

data Response = Response
  { _responseSeq :: ResponseId
  , _responseRequestSeq :: Int
  , _responseSuccess :: Bool
  , _responseCommand :: Text
  , _responseMessage :: Maybe Text
  , _responseBody :: Maybe Value
  }
  deriving (Show)
makeLenses ''Response

instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    _responseSeq <- obj .: "seq"
    _responseSuccess <- obj .: "success"
    _responseRequestSeq <- obj .: "request_seq"
    _responseCommand <- obj .: "command"
    _responseMessage <- obj .:? "message"
    _responseBody <- obj .:? "body"
    pure $ Response {..}
