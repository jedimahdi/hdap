{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dap where

import Colog.Core (LogAction, WithSeverity)
import Colog.Core qualified as L
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Exception
import Control.Monad (forM_, forever, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe
import Dap.Env
import Dap.Event
import Dap.MsgIn qualified as MsgIn
import Dap.MsgOut qualified as MsgOut
import Dap.Protocol
import Dap.Request
import Dap.Response
import Dap.Session
import Dap.Types
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Functor (void)
import Data.IORef
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text
import Data.Text qualified as T
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream qualified as Stream
import Prettyprinter
import System.FilePath (takeDirectory)
import System.Timeout (timeout)
import Text.Pretty.Simple (pPrint)
import UnliftIO.Async
import UnliftIO.STM
import Utils

initialize :: Value -> IO Session
initialize args = do
  (socket, remoteAddr) <- TCP.connectSock "localhost" "8123"
  putStrLn $ "Connection established to " ++ show remoteAddr ++ " Socket: " ++ show socket
  session <- newSession logger socket
  void $ forkFinally (Async.race_ (MsgOut.app session) (MsgIn.app session)) \_ -> do
    putStrLn "Session destroyed"
  waitForInitialized <- expectEvent "initialized" session
  initReqId <- sendRequest makeInitializeRequest session
  Just capabilities <- waitForResposne @InitializeResponseBody initReqId session
  stopped <- waitForInitialized
  configureDoneReqId <- sendRequest makeConfigurationDone session
  x <- waitForResposne @() configureDoneReqId session
  writeIORef session.args args
  pure session
  where
    prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)
    logger :: LogAction IO (WithSeverity DapClientLog)
    logger = L.cmap (show . prettyMsg) L.logStringStderr

startDebugging :: Session -> IO (Session, StartDebuggingRequestArguments)
startDebugging session = do
  args <- readIORef session.args
  waitForStartDebuggin <- expectReverseRequest "startDebugging" session
  launchReqId <- sendRequest (makeLaunchRequest args) session
  maybeStartDebugging <- timeout 2_000_000 waitForStartDebuggin
  case maybeStartDebugging of
    Nothing -> throwIO (AssertionFailed "skldj")
    Just startDebugging -> do
      startDebuggingArgs <- throwNothing startDebugging.arguments >>= throwNothing . fromJSONValue @StartDebuggingRequestArguments
      newSession <- initialize (Object startDebuggingArgs.configuration)
      writeIORef session.isLaunched True
      pure (newSession, startDebuggingArgs)

continue :: Session -> IO ()
continue session = do
  pure ()

-- isLaunched <- readIORef session.isLaunched
-- if isLaunched
--   then do
--     putStrLn "Not implemented yet!!1"
--     pure Nothing
--   else launch
-- where
--   launch = do

setBreakpoint :: Session -> Text -> [Int] -> IO ()
setBreakpoint session filePath bps = do
  setBreakpointReqId <- sendRequest (makeSetBreakpointsRequest filePath [2]) session
  setExceptionBreakpointId <- sendRequest makeSetExceptionBreakpointsRequest session
  configureDoneReqId <- sendRequest makeConfigurationDone session
  waitForResposne @() setBreakpointReqId session
  waitForResposne @() setExceptionBreakpointId session
  waitForResposne @() configureDoneReqId session

-- handleMsg :: DapEnv -> Session -> IO ()
-- handleMsg env session = forever $ do
--   let msgIns = session.msgInChan
--   msg <- atomically $ readTQueue msgIns
--   case msg of
--     ResponseMsg response -> do
--       putStrLn $ show session ++ ": Response to " ++ show response.command
--       print response
--       when response.success
--         $ callCallback session response.requestId response
--     EventMsg event -> do
--       let eventType = event.event
--       when (eventType /= "output" && eventType /= "loadedSource") $ do
--         putStrLn $ show session ++ ": Event " ++ show eventType
--         print event
--       callEventCallback env session eventType event
--       callAfterEventCallbacks env event session eventType
--     RequestMsg request -> do
--       putStrLn $ show session ++ ": Reverse Request " ++ show request.command
--       print request
--       callHandleReverseRequest env request.command request
--
-- setupEvents :: MonadIO m => DapEnv -> m ()
-- setupEvents env = do
--   on env "initialized" $ \(_, session) -> do
--     let filePath = "/home/mahdi/tmp/app.js"
--     let handleSetBreakpointsResponse :: Session -> SetBreakpointsResponse -> HandlerM ()
--         handleSetBreakpointsResponse session body = do
--           atomically $ writeTVar env.breakpoints body.breakpoints
--           sendRequest session makeSetExceptionBreakpointsRequest handleSetExceptionBreakpointsResponse
--
--         handleSetExceptionBreakpointsResponse :: Session -> () -> HandlerM ()
--         handleSetExceptionBreakpointsResponse session _ = do
--           sendRequest session makeConfigurationDone handleConfigureDoneResponse
--
--         handleConfigureDoneResponse :: Session -> () -> HandlerM ()
--         handleConfigureDoneResponse session _ = do
--           atomically $ writeTVar session.isInitialized True
--           atomically $ writeTVar env.session (Just session)
--
--     sendRequest session (makeSetBreakpointsRequest filePath [2]) handleSetBreakpointsResponse
--
--   on env "stopped" $ \(event, session) -> do
--     rawBody <- throwNothing event.body
--     body <- throwNothing $ fromJSONValue @StoppedEventBody rawBody
--     threadId <- throwNothing body.threadId
--     updateThreads session
--     let handleStackTraceResponse :: Session -> StackTraceResponseBody -> HandlerM ()
--         handleStackTraceResponse session body = do
--           let frames = body.stackFrames
--           currentFrame <- hoistMaybe $ getTopFrame frames
--           _thread <- getThread session threadId >>= throwNothing
--           -- requestScopes session currentFrame
--           pure ()
--
--     sendRequest session (makeStackTraceRequest threadId) handleStackTraceResponse
--
--   on env "breakpoint" \(event, _session) -> do
--     body <- throwNothing event.body >>= throwNothing . fromJSONValue @BreakpointEventBody
--     atomically $ modifyTVar' env.breakpoints (fmap (\b -> if b.id == body.breakpoint.id then body.breakpoint else b))
--
--   handleReverseRequest env "startDebugging" $ \request -> do
--     args <- throwNothing request.arguments >>= throwNothing . fromJSONValue @StartDebuggingRequestArguments
--     let configuration = args.configuration
--     let requestType = String args.request
--     let fullConfig = KeyMap.insert "request" requestType configuration
--     initialize env (Object fullConfig)
--
-- next :: DapEnv -> (Session -> HandlerM ()) -> IO ()
-- next env handler = do
--   session <- readTVarIO env.session >>= throwNothing
--   threadsMap <- readTVarIO session.threads
--   case listToMaybe $ Map.elems threadsMap of
--     Nothing -> liftIO $ putStrLn "Stupid !!!"
--     Just (ThreadState id _ _) -> do
--       sendRequest session (makeNextRequest id) (\s () -> handler s)
--
-- requestScopes :: MonadIO m => Session -> StackFrame -> m ()
-- requestScopes session frame = do
--   sendRequest session (makeScopesRequest frame.id) handleScopesResponse
--   where
--     handleScopesResponse :: Session -> ScopesResponseBody -> MaybeT IO ()
--     handleScopesResponse session body = do
--       forM_ body.scopes \scope ->
--         unless scope.expensive
--           $ sendRequest session (makeVariablesRequest scope.variablesReference) handleVariableResponse
--
--     handleVariableResponse :: Session -> VariablesResponseBody -> MaybeT IO ()
--     handleVariableResponse _session body = do
--       let variables = body.variables
--       -- pPrint variables
--       pure ()
--
-- getTopFrame :: [StackFrame] -> Maybe StackFrame
-- getTopFrame frames =
--   case List.find (\s -> isJust s.source) frames of
--     Nothing -> listToMaybe frames
--     x -> x

-- updateThreads :: MonadIO m => Session -> m ()
-- updateThreads session =
--   sendRequest session makeThreadsRequest handleThreadsResponse
--   where
--     handleThreadsResponse :: Session -> ThreadsResponseBody -> HandlerM ()
--     handleThreadsResponse session body = do
--       let threads = body.threads
--       -- oldThreadsMap <- atomically $ readTVar (session ^. sessionThreads)
--       let threadsMap = Map.fromList $ fmap (\(Thread id name) -> (id, ThreadState id name True)) threads
--       atomically $ writeTVar session.threads threadsMap
--
-- on :: MonadIO m => DapEnv -> Text -> EventCallback -> m ()
-- on env event callback =
--   atomically $ modifyTVar' env.eventCallbacks (Map.insert event callback)
--
-- callEventCallback :: MonadIO m => DapEnv -> Session -> Text -> Event -> m ()
-- callEventCallback env session event value = do
--   callbackMap <- readTVarIO env.eventCallbacks
--   case Map.lookup event callbackMap of
--     Just callback -> liftIO $ callback (value, session)
--     Nothing -> pure ()
--
-- handleReverseRequest :: MonadIO m => DapEnv -> Text -> ReverseRequestCallback -> m ()
-- handleReverseRequest env command callback =
--   atomically $ modifyTVar' env.reverseRequestCallbacks (Map.insert command callback)
--
-- callHandleReverseRequest :: MonadIO m => DapEnv -> Text -> Request -> m ()
-- callHandleReverseRequest env event value = do
--   callbackMap <- readTVarIO env.reverseRequestCallbacks
--   case Map.lookup event callbackMap of
--     Just callback -> liftIO $ callback value
--     Nothing -> pure ()
