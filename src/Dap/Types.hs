{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Dap.Types where

import Dap.Event
import Dap.Request (Request)
import Dap.Response (Response)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.TH
import Data.Map (Map)
import Data.Text
import GHC.Generics (Generic)
import Utils

data MsgIn = EventMsg Event | RequestMsg Request | ResponseMsg Response
  deriving (Show)

instance FromJSON MsgIn where
  parseJSON = withObject "MsgIn" $ \obj -> do
    msgType :: Text <- obj .: "type"
    case msgType of
      "event" -> EventMsg <$> parseJSON (Object obj)
      "request" -> RequestMsg <$> parseJSON (Object obj)
      "response" -> ResponseMsg <$> parseJSON (Object obj)
      _ -> fail "Parsing MsgIn failed: wrong msg type"

type MsgOut = Request

data Checksum = Checksum
  { algorithm :: Text
  , checksum :: Text
  }
  deriving (Show, Generic, FromJSON)

data Source = Source
  { name :: Maybe Text
  , path :: Maybe Text
  , sourceReference :: Maybe Int
  , presentationHint :: Maybe Text
  , origin :: Maybe Text
  , sources :: Maybe [Source]
  , adapterData :: Maybe Value
  , checksums :: Maybe [Checksum]
  }
  deriving (Show, Generic, FromJSON)

data StackFrame = StackFrame
  { id :: Int
  , name :: Text
  , source :: Maybe Source
  }
  deriving (Show, Generic, FromJSON)

data Scope = Scope
  { name :: Text
  , presentationHint :: Maybe Text
  , variablesReference :: Int
  , namedVariables :: Maybe Int
  , indexedVariables :: Maybe Int
  , expensive :: Bool
  , source :: Maybe Source
  , line :: Maybe Int
  , column :: Maybe Int
  , endLine :: Maybe Int
  , endColumn :: Maybe Int
  }
  deriving (Show, Generic, FromJSON)

data VariablePresentationHint = VariablePresentationHint
  { kind :: Maybe Text
  , attributes :: Maybe [Text]
  , visibility :: Maybe Text
  , lazy :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON)

data Variable = Variable
  { name :: Text
  , value :: Text
  , variableType :: Maybe Text -- type
  , presentationHint :: Maybe VariablePresentationHint
  , evaluateName :: Maybe Text
  }
instance FromJSON Variable where
  parseJSON = withObject "Variable" $ \obj -> do
    name <- obj .: "name"
    value <- obj .: "value"
    variableType <- obj .:? "type"
    presentationHint <- obj .:? "presentationHint"
    evaluateName <- obj .:? "evaluateName"
    pure $ Variable {..}

data Thread = Thread
  { id :: Int
  , name :: Text
  }
  deriving (Show, Generic, FromJSON)

data StartDebuggingRequestArguments = StartDebuggingRequestArguments
  { configuration :: KeyMap Value
  , request :: Text
  }
  deriving (Show, Generic, FromJSON)

data ThreadsResponseBody = ThreadsResponseBody
  { threads :: [Thread]
  }

instance FromJSON ThreadsResponseBody where
  parseJSON = withObject "ThreadsResponseBody" $ \obj -> do
    threads <- obj .: "threads"
    pure $ ThreadsResponseBody {..}

data StackTraceResponseBody = StackTraceResponseBody
  { stackFrames :: [StackFrame]
  , totalFrames :: Maybe Int
  }
  deriving (Show, Generic, FromJSON)

data StoppedEventBody = StoppedEventBody
  { reason :: Text
  , description :: Maybe Text
  , threadId :: Maybe Int
  , preserveFocusHint :: Maybe Bool
  , text :: Maybe Text
  , allThreadsStopped :: Maybe Bool
  , hitBreakpointIds :: Maybe [Int]
  }
  deriving (Show, Generic, FromJSON)
