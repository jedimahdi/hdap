{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Dap.Types where

import Dap.Event
import Dap.Request (Request)
import Dap.Response (Response)
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Haskell.DAP qualified as DAP
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

data Source = Source
  { _name :: Maybe Text
  , _path :: Maybe Text
  , _sourceReference :: Maybe Int
  , _presentationHint :: Maybe Text
  , _origin :: Maybe Text
  , _sources :: Maybe [Source]
  , _adapterData :: Maybe Value
  , _checksums :: Maybe [Checksum]
  }

data StackFrame = StackFrame
  { _id :: Int
  , _name :: Text
  , _source :: Maybe Source
  }

data Scope = Scope
  { _name :: Text
  , _presentationHint :: Maybe Text
  , _variablesReference :: Int
  , _namedVariables :: Maybe Int
  , _indexedVariables :: Maybe Int
  , _expensive :: Bool
  , _source :: Maybe Source
  , _line :: Maybe Int
  , _column :: Maybe Int
  , _endLine :: Maybe Int
  , _endColumn :: Maybe Int
  }

data VariablePresentationHint = VariablePresentationHint
  { _kind :: Maybe Text
  , _attributes :: Maybe [Text]
  , _visibility :: Maybe Text
  , _lazy :: Maybe Bool
  }

data Variable = Variable
  { _name :: Text
  , _value :: Text
  , _type :: Maybe Text
  , _presentationHint :: Maybe VariablePresentationHint
  , _evaluateName :: Maybe Text
  }

data Thread = Thread
  { id :: Int
  , name :: Text
  }
instance FromJSON Thread where
  parseJSON = withObject "Thread" $ \obj -> do
    id <- obj .: "id"
    name <- obj .: "name"
    pure $ Thread {..}

data ThreadsResponseBody = ThreadsResponseBody
  { threads :: [Thread]
  }

instance FromJSON ThreadsResponseBody where
  parseJSON = withObject "ThreadsResponseBody" $ \obj -> do
    threads <- obj .: "threads"
    pure $ ThreadsResponseBody {..}

-- case text of
--   "user"             -> return User
--   "admin"            -> return Admin
--   "customer_support" -> return CustomerSupport
--   _                  -> fail "string is not one of known enum values"

-- data EventType
--   = DapEventOutput
--   | DapEventStopped
--   | DapEventLoadedSource
--   | DapEventInitialized
--   deriving (Eq, Ord)
--
-- parseEventType :: Text -> Maybe DapEventType
-- parseEventType "output" = Just DapEventOutput
-- parseEventType "stopped" = Just DapEventStopped
-- parseEventType "loadedSource" = Just DapEventLoadedSource
-- parseEventType "initialized" = Just DapEventInitialized
-- parseEventType _ = Nothing

-- data Request
--   = InitializeRequest DAP.InitializeRequest
--   | LaunchRequest DAP.LaunchRequest
--   | DisconnectRequest DAP.DisconnectRequest
--   | PauseRequest DAP.PauseRequest
--   | TerminateRequest DAP.TerminateRequest
--   | SetBreakpointsRequest DAP.SetBreakpointsRequest
--   | SetFunctionBreakpointsRequest DAP.SetFunctionBreakpointsRequest
--   | SetExceptionBreakpointsRequest DAP.SetExceptionBreakpointsRequest
--   | ConfigurationDoneRequest DAP.ConfigurationDoneRequest
--   | ThreadsRequest DAP.ThreadsRequest
--   | StackTraceRequest DAP.StackTraceRequest
--   | ScopesRequest DAP.ScopesRequest
--   | VariablesRequest DAP.VariablesRequest
--   | SourceRequest DAP.SourceRequest
--   | ContinueRequest DAP.ContinueRequest
--   | NextRequest DAP.NextRequest
--   | StepInRequest DAP.StepInRequest
--   | EvaluateRequest DAP.EvaluateRequest
--   | CompletionsRequest DAP.CompletionsRequest
--   deriving (Show, Read, Eq)
--
-- data MsgIn
--
-- data EventType = InitializedEvent | OutputEvent

-- data Event = Event
--   { _event :: EventType
--   , _body :: Value
--   }

-- data Response
--   = InitializeResponse DAP.InitializeResponse
--   | LaunchResponse DAP.LaunchResponse
--   | OutputEvent DAP.OutputEvent
--   | StoppedEvent DAP.StoppedEvent
--   | TerminatedEvent DAP.TerminatedEvent
--   | ExitedEvent DAP.ExitedEvent
--   | ContinuedEvent DAP.ContinuedEvent
--   | InitializedEvent DAP.InitializedEvent
--   | DisconnectResponse DAP.DisconnectResponse
--   | PauseResponse DAP.PauseResponse
--   | TerminateResponse DAP.TerminateResponse
--   | SetBreakpointsResponse DAP.SetBreakpointsResponse
--   | SetFunctionBreakpointsResponse DAP.SetFunctionBreakpointsResponse
--   | SetExceptionBreakpointsResponse DAP.SetExceptionBreakpointsResponse
--   | ConfigurationDoneResponse DAP.ConfigurationDoneResponse
--   | ThreadsResponse DAP.ThreadsResponse
--   | StackTraceResponse DAP.StackTraceResponse
--   | ScopesResponse DAP.ScopesResponse
--   | VariablesResponse DAP.VariablesResponse
--   | SourceResponse DAP.SourceResponse
--   | ContinueResponse DAP.ContinueResponse
--   | NextResponse DAP.NextResponse
--   | StepInResponse DAP.StepInResponse
--   | EvaluateResponse DAP.EvaluateResponse
--   | CompletionsResponse DAP.CompletionsResponse
--   deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Source", omitNothingFields = True} ''DAP.Source)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceBreakpoint", omitNothingFields = True} ''DAP.SourceBreakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Breakpoint", omitNothingFields = True} ''DAP.Breakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "FunctionBreakpoint", omitNothingFields = True} ''DAP.FunctionBreakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Thread", omitNothingFields = True} ''DAP.Thread)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackFrame", omitNothingFields = True} ''DAP.StackFrame)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Scope", omitNothingFields = True} ''DAP.Scope)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablePresentationHint", omitNothingFields = True} ''DAP.VariablePresentationHint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Variable", omitNothingFields = True} ''DAP.Variable)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsItem", omitNothingFields = True} ''DAP.CompletionsItem)

-- jsonize
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Request", omitNothingFields = True} ''DAP.Request)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeRequestArguments", omitNothingFields = True} ''DAP.InitializeRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeRequest", omitNothingFields = True} ''DAP.InitializeRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchRequestArguments", omitNothingFields = True} ''DAP.LaunchRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchRequest", omitNothingFields = True} ''DAP.LaunchRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectRequestArguments", omitNothingFields = True} ''DAP.DisconnectRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectRequest", omitNothingFields = True} ''DAP.DisconnectRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseRequestArguments", omitNothingFields = True} ''DAP.PauseRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseRequest", omitNothingFields = True} ''DAP.PauseRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateRequestArguments", omitNothingFields = True} ''DAP.TerminateRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateRequest", omitNothingFields = True} ''DAP.TerminateRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsRequestArguments", omitNothingFields = True} ''DAP.SetBreakpointsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsRequest", omitNothingFields = True} ''DAP.SetBreakpointsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsRequestArguments", omitNothingFields = True} ''DAP.SetFunctionBreakpointsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsRequest", omitNothingFields = True} ''DAP.SetFunctionBreakpointsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsRequestArguments", omitNothingFields = True} ''DAP.SetExceptionBreakpointsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsRequest", omitNothingFields = True} ''DAP.SetExceptionBreakpointsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ConfigurationDoneRequest", omitNothingFields = True} ''DAP.ConfigurationDoneRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsRequest", omitNothingFields = True} ''DAP.ThreadsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceRequestArguments", omitNothingFields = True} ''DAP.StackTraceRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceRequest", omitNothingFields = True} ''DAP.StackTraceRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesRequestArguments", omitNothingFields = True} ''DAP.ScopesRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesRequest", omitNothingFields = True} ''DAP.ScopesRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesRequestArguments", omitNothingFields = True} ''DAP.VariablesRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesRequest", omitNothingFields = True} ''DAP.VariablesRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceRequestArguments", omitNothingFields = True} ''DAP.SourceRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceRequest", omitNothingFields = True} ''DAP.SourceRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueRequestArguments", omitNothingFields = True} ''DAP.ContinueRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueRequest", omitNothingFields = True} ''DAP.ContinueRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextRequestArguments", omitNothingFields = True} ''DAP.NextRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextRequest", omitNothingFields = True} ''DAP.NextRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInRequestArguments", omitNothingFields = True} ''DAP.StepInRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInRequest", omitNothingFields = True} ''DAP.StepInRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateRequestArguments", omitNothingFields = True} ''DAP.EvaluateRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateRequest", omitNothingFields = True} ''DAP.EvaluateRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsRequestArguments", omitNothingFields = True} ''DAP.CompletionsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsRequest", omitNothingFields = True} ''DAP.CompletionsRequest)

-- Response
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Response", omitNothingFields = True} ''DAP.Response)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ColumnDescriptor", omitNothingFields = True} ''DAP.ColumnDescriptor)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExceptionBreakpointsFilter", omitNothingFields = True} ''DAP.ExceptionBreakpointsFilter)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeResponseBody", omitNothingFields = True} ''DAP.InitializeResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeResponse", omitNothingFields = True} ''DAP.InitializeResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchResponse", omitNothingFields = True} ''DAP.LaunchResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "OutputEventBody", omitNothingFields = True} ''DAP.OutputEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "OutputEvent", omitNothingFields = True} ''DAP.OutputEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StoppedEventBody", omitNothingFields = True} ''DAP.StoppedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StoppedEvent", omitNothingFields = True} ''DAP.StoppedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializedEvent", omitNothingFields = True} ''DAP.InitializedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectResponse", omitNothingFields = True} ''DAP.DisconnectResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseResponse", omitNothingFields = True} ''DAP.PauseResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateResponse", omitNothingFields = True} ''DAP.TerminateResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsResponseBody", omitNothingFields = True} ''DAP.SetBreakpointsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsResponse", omitNothingFields = True} ''DAP.SetBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsResponseBody", omitNothingFields = True} ''DAP.SetFunctionBreakpointsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsResponse", omitNothingFields = True} ''DAP.SetFunctionBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsResponse", omitNothingFields = True} ''DAP.SetExceptionBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ConfigurationDoneResponse", omitNothingFields = True} ''DAP.ConfigurationDoneResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsResponseBody", omitNothingFields = True} ''DAP.ThreadsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsResponse", omitNothingFields = True} ''DAP.ThreadsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceResponseBody", omitNothingFields = True} ''DAP.StackTraceResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceResponse", omitNothingFields = True} ''DAP.StackTraceResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesResponseBody", omitNothingFields = True} ''DAP.ScopesResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesResponse", omitNothingFields = True} ''DAP.ScopesResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesResponseBody", omitNothingFields = True} ''DAP.VariablesResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesResponse", omitNothingFields = True} ''DAP.VariablesResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceResponseBody", omitNothingFields = True} ''DAP.SourceResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceResponse", omitNothingFields = True} ''DAP.SourceResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueResponse", omitNothingFields = True} ''DAP.ContinueResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextResponse", omitNothingFields = True} ''DAP.NextResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInResponse", omitNothingFields = True} ''DAP.StepInResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateResponseBody", omitNothingFields = True} ''DAP.EvaluateResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateResponse", omitNothingFields = True} ''DAP.EvaluateResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsResponseBody", omitNothingFields = True} ''DAP.CompletionsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsResponse", omitNothingFields = True} ''DAP.CompletionsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminatedEventBody", omitNothingFields = True} ''DAP.TerminatedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminatedEvent", omitNothingFields = True} ''DAP.TerminatedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExitedEventBody", omitNothingFields = True} ''DAP.ExitedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExitedEvent", omitNothingFields = True} ''DAP.ExitedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinuedEventBody", omitNothingFields = True} ''DAP.ContinuedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinuedEvent", omitNothingFields = True} ''DAP.ContinuedEvent)

-- $(deriveJSON defaultOptions {sumEncoding = UntaggedValue} ''Request)

-- $(deriveJSON defaultOptions {sumEncoding = UntaggedValue} ''Response)
