{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Dap.Types where

import Control.Monad.Trans.Maybe (MaybeT)
import Dap.Event
import Dap.Response (Response)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.TH
import Data.Map (Map)
import Data.Text
import GHC.Generics (Generic)
import Utils

type RequestId = Int

data Request = Request
  { id :: RequestId
  , arguments :: Maybe Value
  , command :: Text
  }
  deriving (Show)

instance ToJSON Request where
  toJSON (Request i margs command) =
    case margs of
      Just args ->
        object
          [ "seq" .= i
          , "type" .= String "request"
          , "command" .= command
          , "arguments" .= args
          ]
      Nothing ->
        object ["seq" .= i, "type" .= String "request", "command" .= command]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    id <- obj .: "seq"
    command <- obj .: "command"
    arguments <- obj .:? "arguments"
    pure $ Request {..}

type HandlerM a = MaybeT IO a

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

data Breakpoint = Breakpoint
  { id :: Maybe Int
  , verified :: Bool
  , message :: Maybe Text
  , source :: Maybe Source
  , line :: Maybe Int
  , column :: Maybe Int
  , endLine :: Maybe Int
  , endColumn :: Maybe Int
  , instructionReference :: Maybe Text
  , offset :: Maybe Int
  , reason :: Maybe Text
  }
  deriving (Show, Generic, FromJSON)

type ChecksumAlgorithm = Text

data Checksum = Checksum
  { algorithm :: ChecksumAlgorithm
  , checksum :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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
  deriving (Show, Generic, FromJSON, ToJSON)

data StackFrame = StackFrame
  { id :: Int
  , name :: Text
  , source :: Maybe Source
  , line :: Int
  , column :: Int
  , endLine :: Maybe Int
  , endColumn :: Maybe Int
  , canRestart :: Maybe Bool
  , instructionPointerReference :: Maybe Text
  , moduleId :: Maybe Value
  , presentationHint :: Maybe Text
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

data Capabilities = Capabilities
  { supportsConfigurationDoneRequest :: Maybe Bool
  , supportsFunctionBreakpoints :: Maybe Bool
  , supportsConditionalBreakpoints :: Maybe Bool
  , supportsHitConditionalBreakpoints :: Maybe Bool
  , supportsEvaluateForHovers :: Maybe Bool
  , exceptionBreakpointFilters :: Maybe [ExceptionBreakpointsFilter]
  , supportsStepBack :: Maybe Bool
  , supportsSetVariable :: Maybe Bool
  , supportsRestartFrame :: Maybe Bool
  , supportsGotoTargetsRequest :: Maybe Bool
  , supportsStepInTargetsRequest :: Maybe Bool
  , supportsCompletionsRequest :: Maybe Bool
  , completionTriggerCharacters :: Maybe [Text]
  , supportsModulesRequest :: Maybe Bool
  , additionalModuleColumns :: Maybe [ColumnDescriptor]
  , supportedChecksumAlgorithms :: Maybe [ChecksumAlgorithm]
  , supportsRestartRequest :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON)

data ColumnDescriptor = ColumnDescriptor
  { attributeName :: Text
  , label :: Text
  , format :: Maybe Text
  , _type :: Maybe Text -- type
  , width :: Maybe Int
  }
  deriving (Show, Generic)
instance FromJSON ColumnDescriptor where
  parseJSON = withObject "ColumnDescriptor" $ \obj -> do
    attributeName <- obj .: "attributeName"
    label <- obj .: "label"
    format <- obj .:? "format"
    _type <- obj .:? "type"
    width <- obj .:? "width"
    pure $ ColumnDescriptor {..}

data ExceptionBreakpointsFilter = ExceptionBreakpointsFilter
  { filter :: Text
  , label :: Text
  , description :: Maybe Text
  , _default :: Maybe Bool -- default
  , supportsCondition :: Maybe Bool
  , conditionDescription :: Maybe Text
  }
  deriving (Show, Generic)
instance FromJSON ExceptionBreakpointsFilter where
  parseJSON = withObject "ExceptionBreakpointsFilter" $ \obj -> do
    filter <- obj .: "filter"
    label <- obj .: "label"
    description <- obj .:? "description"
    _default <- obj .:? "default"
    supportsCondition <- obj .:? "supportsCondition"
    conditionDescription <- obj .:? "conditionDescription"
    pure $ ExceptionBreakpointsFilter {..}

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
  , _type :: Maybe Text -- type
  , presentationHint :: Maybe VariablePresentationHint
  , evaluateName :: Maybe Text
  }
  deriving (Show, Generic)
instance FromJSON Variable where
  parseJSON = withObject "Variable" $ \obj -> do
    name <- obj .: "name"
    value <- obj .: "value"
    _type <- obj .:? "type"
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

data ScopesResponseBody = ScopesResponseBody
  { scopes :: [Scope]
  }
  deriving (Show, Generic, FromJSON)

data VariablesResponseBody = VariablesResponseBody
  { variables :: [Variable]
  }
  deriving (Show, Generic, FromJSON)

data SetBreakpointsResponse = SetBreakpointsResponse
  { breakpoints :: [Breakpoint]
  }
  deriving (Show, Generic, FromJSON)

data SourceResponseBody = SourceResponseBody
  { content :: Text
  , mimeType :: Maybe Text
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

data BreakpointEventBody = BreakpointEventBody
  { reason :: Text
  , breakpoint :: Breakpoint
  }
  deriving (Show, Generic, FromJSON)
