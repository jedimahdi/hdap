{-# LANGUAGE TemplateHaskell #-}
module Dap.Data.Request where

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
import System.FilePath (takeDirectory, takeFileName)

type RequestId = Int

data Request = Request
  { _requestSeq :: RequestId
  , _requestArguments :: Maybe Object
  , _requestCommand :: Text
  }
  deriving (Show)
makeLenses ''Request

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

makeRequest :: Text -> Aeson.Object -> RequestId -> Request
makeRequest command arguments requestId =
  Request
    { _requestSeq = requestId
    , _requestCommand = command
    , _requestArguments = Just arguments
    }

makeRequest' :: Text -> RequestId -> Request
makeRequest' command requestId =
  Request
    { _requestSeq = requestId
    , _requestCommand = command
    , _requestArguments = Nothing
    }

makeStackTraceRequest :: Int -> RequestId -> Request
makeStackTraceRequest threadId requestId =
  let args = KeyMap.fromList ["threadId" .= threadId]
   in makeRequest "stackTrace" args requestId

makeScopesRequest :: Int -> RequestId -> Request
makeScopesRequest frameId requestId =
  let args = KeyMap.fromList ["frameId" .= frameId]
   in makeRequest "scopes" args requestId

makeLoadedSourcesRequest :: RequestId -> Request
makeLoadedSourcesRequest requestId = do
  let args = KeyMap.fromList []
  makeRequest "loadedSources" args requestId

makeBreakpointLocationsRequest :: Text -> RequestId -> Request
makeBreakpointLocationsRequest path requestId =
  let args =
        KeyMap.fromList
          [ "source" .= object ["path" .= path, "name" .= takeFileName (T.unpack path)]
          , "line" .= Number 1
          ]
   in makeRequest "breakpointLocations" args requestId

makeSetExceptionBreakpointsRequest :: RequestId -> Request
makeSetExceptionBreakpointsRequest requestId =
  let args =
        KeyMap.fromList
          [("filters", Array (Vector.fromList []))]
   in makeRequest "setExceptionBreakpoints" args requestId

makeSourceRequest :: Text -> RequestId -> Request
makeSourceRequest path requestId =
  let args =
        KeyMap.fromList
          [("source", Object (KeyMap.fromList [("path", String path)]))]
   in makeRequest "source" args requestId

makeContinueRequest :: RequestId -> Request
makeContinueRequest requestId =
  let args =
        KeyMap.fromList
          [("threadId", Number 1)]
   in makeRequest "continue" args requestId

makeThreadsRequest :: RequestId -> Request
makeThreadsRequest = makeRequest' "threads"

makeConfigurationDone :: RequestId -> Request
makeConfigurationDone requestId =
  let args =
        KeyMap.fromList
          []
   in makeRequest "configurationDone" args requestId

makeStartDebuggingRequest :: FilePath -> RequestId -> Request
makeStartDebuggingRequest path requestId =
  let configs =
        KeyMap.fromList
          [ ("request", String "launch")
          , ("type", String "pwa-node")
          , ("name", String "Launch file")
          , ("program", String (T.pack path))
          , ("cwd", String (T.pack (takeDirectory path)))
          ]
      args =
        KeyMap.fromList
          [("configuration", Object configs), ("request", "launch")]
          `KeyMap.union` configs
   in makeRequest "startDebugging" args requestId

makeLaunchRequest :: KeyMap Value -> RequestId -> Request
makeLaunchRequest = makeRequest "launch"

makeInitializeRequest :: RequestId -> Request
makeInitializeRequest requestId =
  let args =
        KeyMap.fromList
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
    KeyMap.fromList
      [
        ( "source"
        , Object $
            KeyMap.fromList
              [ ("path", String path)
              , ("name", String $ T.pack $ takeFileName (T.unpack path))
              ]
        )
      , ("sourceModified", Bool False)
      ,
        ( "breakpoints"
        , Array
            ( ( \line ->
                  Object $
                    KeyMap.fromList
                      [ ("line", Number (fromIntegral line))
                      ]
              )
                <$> Vector.fromList breakpoints
            )
        )
      , ("lines", Array (Number . fromIntegral <$> Vector.fromList breakpoints))
      ]
