{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dap where

import Control.Exception.Safe (MonadThrow)
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
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text
import Data.Text qualified as T
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory)
import Text.Pretty.Simple (pPrint)
import UnliftIO.Async
import UnliftIO.STM
import Utils

initialize :: MonadIO m => DapEnv -> Value -> m ()
initialize env args = do
  setupEvents env
  (socket, remoteAddr) <- TCP.connectSock "localhost" "8123"
  liftIO $ putStrLn $ "Connection established to " ++ show remoteAddr ++ " Socket: " <> show socket
  sessionId <- getNextSessionId env
  session <- newSession sessionId socket
  let sessionVar = env.session
  atomically $ writeTVar sessionVar (Just session)
  void $ liftIO $ do
    void $ async $ MsgOut.app session
    void $ async $ MsgIn.app session
    void $ async $ handleMsg env session
  sessionInitialize session
  where
    sessionInitialize :: MonadIO m => Session -> m ()
    sessionInitialize session = do
      sendRequest session makeInitializeRequest handleInitializeResponse

    handleInitializeResponse :: Session -> () -> HandlerM ()
    handleInitializeResponse session _ = do
      -- TODO: save capabilities
      sendRequest session (makeLaunchRequest args) handleLaunchResponse

    handleLaunchResponse :: Session -> () -> HandlerM ()
    handleLaunchResponse _ _ = do
      pure ()

handleMsg :: DapEnv -> Session -> IO ()
handleMsg env session = forever $ do
  let msgIns = session.msgInChan
  msg <- atomically $ readTQueue msgIns
  case msg of
    ResponseMsg response -> do
      putStrLn $ show session ++ ": Response to " ++ show response.command
      print response
      when response.success
        $ callCallback session response.requestId response
    EventMsg event -> do
      let eventType = event.event
      when (eventType /= "output" && eventType /= "loadedSource") $ do
        putStrLn $ show session ++ ": Event " ++ show eventType
        print event
      callEventCallback env session eventType event
      callAfterEventCallbacks env event session eventType
    RequestMsg request -> do
      putStrLn $ show session ++ ": Reverse Request " ++ show request.command
      print request
      callHandleReverseRequest env request.command request

setupEvents :: MonadIO m => DapEnv -> m ()
setupEvents env = do
  on env "initialized" $ \(_, session) -> do
    let filePath = "/home/mahdi/tmp/app.js"
    let handleSetBreakpointsResponse :: Session -> SetBreakpointsResponse -> HandlerM ()
        handleSetBreakpointsResponse session body = do
          atomically $ writeTVar env.breakpoints body.breakpoints
          sendRequest session makeSetExceptionBreakpointsRequest handleSetExceptionBreakpointsResponse

        handleSetExceptionBreakpointsResponse :: Session -> () -> HandlerM ()
        handleSetExceptionBreakpointsResponse session _ = do
          sendRequest session makeConfigurationDone handleConfigureDoneResponse

        handleConfigureDoneResponse :: Session -> () -> HandlerM ()
        handleConfigureDoneResponse session _ = do
          atomically $ writeTVar session.isInitialized True
          atomically $ writeTVar env.session (Just session)

    sendRequest session (makeSetBreakpointsRequest filePath [2]) handleSetBreakpointsResponse

  on env "stopped" $ \(event, session) -> do
    rawBody <- throwNothing event.body
    body <- throwNothing $ fromJSONValue @StoppedEventBody rawBody
    threadId <- throwNothing body.threadId
    updateThreads session
    let handleStackTraceResponse :: Session -> StackTraceResponseBody -> HandlerM ()
        handleStackTraceResponse session body = do
          let frames = body.stackFrames
          currentFrame <- hoistMaybe $ getTopFrame frames
          _thread <- getThread session threadId >>= throwNothing
          -- requestScopes session currentFrame
          pure ()

    sendRequest session (makeStackTraceRequest threadId) handleStackTraceResponse

  on env "breakpoint" \(event, _session) -> do
    body <- throwNothing event.body >>= throwNothing . fromJSONValue @BreakpointEventBody
    atomically $ modifyTVar' env.breakpoints (fmap (\b -> if b.id == body.breakpoint.id then body.breakpoint else b))

  handleReverseRequest env "startDebugging" $ \request -> do
    args <- throwNothing request.arguments >>= throwNothing . fromJSONValue @StartDebuggingRequestArguments
    let configuration = args.configuration
    let requestType = String args.request
    let fullConfig = KeyMap.insert "request" requestType configuration
    initialize env (Object fullConfig)

next :: DapEnv -> (Session -> HandlerM ()) -> IO ()
next env handler = do
  session <- readTVarIO env.session >>= throwNothing
  threadsMap <- readTVarIO session.threads
  case listToMaybe $ Map.elems threadsMap of
    Nothing -> liftIO $ putStrLn "Stupid !!!"
    Just (ThreadState id _ _) -> do
      sendRequest session (makeNextRequest id) (\s () -> handler s)

requestScopes :: MonadIO m => Session -> StackFrame -> m ()
requestScopes session frame = do
  sendRequest session (makeScopesRequest frame.id) handleScopesResponse
  where
    handleScopesResponse :: Session -> ScopesResponseBody -> MaybeT IO ()
    handleScopesResponse session body = do
      forM_ body.scopes \scope ->
        unless scope.expensive
          $ sendRequest session (makeVariablesRequest scope.variablesReference) handleVariableResponse

    handleVariableResponse :: Session -> VariablesResponseBody -> MaybeT IO ()
    handleVariableResponse _session body = do
      let variables = body.variables
      -- pPrint variables
      pure ()

getTopFrame :: [StackFrame] -> Maybe StackFrame
getTopFrame frames =
  case List.find (\s -> isJust s.source) frames of
    Nothing -> listToMaybe frames
    x -> x

updateThreads :: MonadIO m => Session -> m ()
updateThreads session =
  sendRequest session makeThreadsRequest handleThreadsResponse
  where
    handleThreadsResponse :: Session -> ThreadsResponseBody -> HandlerM ()
    handleThreadsResponse session body = do
      let threads = body.threads
      -- oldThreadsMap <- atomically $ readTVar (session ^. sessionThreads)
      let threadsMap = Map.fromList $ fmap (\(Thread id name) -> (id, ThreadState id name True)) threads
      atomically $ writeTVar session.threads threadsMap

on :: MonadIO m => DapEnv -> Text -> EventCallback -> m ()
on env event callback =
  atomically $ modifyTVar' env.eventCallbacks (Map.insert event callback)

callEventCallback :: MonadIO m => DapEnv -> Session -> Text -> Event -> m ()
callEventCallback env session event value = do
  callbackMap <- readTVarIO env.eventCallbacks
  case Map.lookup event callbackMap of
    Just callback -> liftIO $ callback (value, session)
    Nothing -> pure ()

handleReverseRequest :: MonadIO m => DapEnv -> Text -> ReverseRequestCallback -> m ()
handleReverseRequest env command callback =
  atomically $ modifyTVar' env.reverseRequestCallbacks (Map.insert command callback)

callHandleReverseRequest :: MonadIO m => DapEnv -> Text -> Request -> m ()
callHandleReverseRequest env event value = do
  callbackMap <- readTVarIO env.reverseRequestCallbacks
  case Map.lookup event callbackMap of
    Just callback -> liftIO $ callback value
    Nothing -> pure ()
