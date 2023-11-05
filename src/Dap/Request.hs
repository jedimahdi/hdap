module Dap.Request where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import System.FilePath (takeDirectory, takeFileName)

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

makeRequest :: Text -> Value -> RequestId -> Request
makeRequest command arguments requestId =
  Request
    { id = requestId
    , command = command
    , arguments = Just arguments
    }

makeRequest' :: Text -> RequestId -> Request
makeRequest' command requestId =
  Request
    { id = requestId
    , command = command
    , arguments = Nothing
    }

makeStackTraceRequest :: Int -> RequestId -> Request
makeStackTraceRequest threadId requestId =
  let args = object ["threadId" .= threadId]
   in makeRequest "stackTrace" args requestId

makeScopesRequest :: Int -> RequestId -> Request
makeScopesRequest frameId requestId =
  let args = object ["frameId" .= frameId]
   in makeRequest "scopes" args requestId

makeVariablesRequest :: Int -> RequestId -> Request
makeVariablesRequest variablesReference requestId =
  let args = object ["variablesReference" .= variablesReference]
   in makeRequest "variables" args requestId

makeLoadedSourcesRequest :: RequestId -> Request
makeLoadedSourcesRequest requestId = do
  let args = object []
  makeRequest "loadedSources" args requestId

makeBreakpointLocationsRequest :: Text -> RequestId -> Request
makeBreakpointLocationsRequest path requestId =
  let args =
        object
          [ "source" .= object ["path" .= path, "name" .= takeFileName (T.unpack path)]
          , "line" .= Number 1
          ]
   in makeRequest "breakpointLocations" args requestId

makeSetExceptionBreakpointsRequest :: RequestId -> Request
makeSetExceptionBreakpointsRequest requestId =
  let args =
        object
          [("filters", Array (Vector.fromList []))]
   in makeRequest "setExceptionBreakpoints" args requestId

makeSourceRequest :: Text -> RequestId -> Request
makeSourceRequest path requestId =
  let args =
        object
          [("source", Object (KeyMap.fromList [("path", String path)]))]
   in makeRequest "source" args requestId

makeContinueRequest :: RequestId -> Request
makeContinueRequest requestId =
  let args =
        object
          [("threadId", Number 1)]
   in makeRequest "continue" args requestId

makeThreadsRequest :: RequestId -> Request
makeThreadsRequest = makeRequest' "threads"

makeConfigurationDone :: RequestId -> Request
makeConfigurationDone requestId =
  let args =
        object
          []
   in makeRequest "configurationDone" args requestId

makeLaunchRequest :: Value -> RequestId -> Request
makeLaunchRequest = makeRequest "launch"

makeInitializeRequest :: RequestId -> Request
makeInitializeRequest requestId =
  let args =
        object
          [ ("clientId", String "dap-tui")
          , ("clientname", String "dap-tui")
          , ("supportsStartDebuggingRequest", Bool False)
          , ("supportsRunInTerminalRequest", Bool True)
          , ("supportsProgressReporting", Bool True)
          , ("supportsVariableType", Bool True)
          , ("pathFormat", "path")
          , ("locale", "en_US.UTF-8")
          , ("columnsStartAt1", Bool True)
          , ("linesStartAt1", Bool True)
          , ("adapterID", String "dap-tui")
          ]
   in makeRequest "initialize" args requestId

makeSetBreakpointsRequest :: Text -> [Int] -> RequestId -> Request
makeSetBreakpointsRequest path breakpoints = makeRequest "setBreakpoints" arguments
  where
    arguments =
      object
        [
          ( "source"
          , Object
              $ KeyMap.fromList
                [ ("path", String path)
                , ("name", String $ T.pack $ takeFileName (T.unpack path))
                ]
          )
        , ("sourceModified", Bool False)
        ,
          ( "breakpoints"
          , Array
              ( ( \line ->
                    Object
                      $ KeyMap.fromList
                        [ ("line", Number (fromIntegral line))
                        ]
                )
                  <$> Vector.fromList breakpoints
              )
          )
        , ("lines", Array (Number . fromIntegral <$> Vector.fromList breakpoints))
        ]
