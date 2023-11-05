{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dap where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (forever, unless, when)
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
import Data.Map qualified as Map
import Data.Text
import Data.Text qualified as T
import Haskell.DAP qualified as DAP
import Lens.Micro
import Lens.Micro.Aeson
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory)
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
  let sessionVar = env ^. envSession
  atomically $ writeTVar sessionVar (Just session)
  void $ liftIO $ do
    void $ async $ MsgOut.app session
    void $ async $ MsgIn.app session
    void $ async $ handleMsg env session
  sessionInitialize session args
  where
    sessionInitialize :: MonadIO m => Session -> Value -> m ()
    sessionInitialize session args = do
      sendRequest session makeInitializeRequest \_ -> do
        -- TODO: save capabilities
        sendRequest session (makeLaunchRequest args) \_ -> do
          pure ()

getNextSessionId :: MonadIO m => DapEnv -> m Int
getNextSessionId env = do
  let nextSessionIdVar = env ^. nextSessionId
  atomically $ do
    i <- readTVar nextSessionIdVar
    writeTVar nextSessionIdVar (i + 1)
    pure i

handleMsg :: DapEnv -> Session -> IO ()
handleMsg env session = forever $ do
  let msgIns = session ^. msgInChan
  msg <- atomically $ readTQueue msgIns
  case msg of
    ResponseMsg response -> do
      let responseRequestId = response ^. responseRequestSeq
      let command = response ^. responseCommand
      putStrLn $ show session ++ ": Response to " ++ show command
      print response
      callCallback session responseRequestId response
    EventMsg event -> do
      let eventType = event ^. eventEvent
      when (eventType /= "output" && eventType /= "loadedSource") $ do
        putStrLn $ show session ++ ": Event " ++ show eventType
        print event
      callEventCallback env session eventType event
    RequestMsg request -> do
      let command = request ^. requestCommand
      putStrLn $ show session ++ ": Reverse Request " ++ show command
      print request
      callHandleReverseRequest env command request

setupEvents :: MonadIO m => DapEnv -> m ()
setupEvents env = do
  on env "initialized" $ \(_, session) -> do
    let filePath = "/home/mahdi/tmp/app.js"
    sendRequest session (makeSetBreakpointsRequest filePath [2]) $ \_ -> do
      sendRequest session makeSetExceptionBreakpointsRequest $ \_ -> do
        sendRequest session makeConfigurationDone $ \_ -> do
          atomically $ writeTVar (session ^. isInitialized) True
          atomically $ writeTVar (env ^. envSession) (Just session)

  on env "stopped" $ \(event, session) -> do
    -- allThreadsStopped <- throwNothing $ event ^? eventBody . _Just . key "allThreadsStopped" . _Bool
    putStrLn "before everything"
    threadId :: Int <- throwNothing $ event ^? eventBody . _Just . key "threadId" . _Number . _Integral
    putStrLn "before updateThreads"
    updateThreads session
    putStrLn "After updateThreads"
    sendRequest session (makeStackTraceRequest threadId) $ \response -> do
      body <- throwNothing $ response ^. responseBody
      stackTraceResponseBody <- throwNothing $ fromJSONValue @DAP.StackTraceResponseBody body
      let frames = DAP.stackFramesStackTraceResponseBody stackTraceResponseBody
      pure ()

  handleReverseRequest env "startDebugging" $ \request -> do
    configuration <- throwNothing $ request ^? requestArguments . _Just . key "configuration" . _Object
    requestType <- throwNothing $ request ^? requestArguments . _Just . key "request"
    let fullConfig = KeyMap.insert "request" requestType configuration
    initialize env (Object fullConfig)

-- getTopFrame :: [DAP.StackFrame] -> Maybe (DAP.StackFrame)
-- getTopFrame frames = List.find

updateThreads :: MonadIO m => Session -> m ()
updateThreads session =
  sendRequest session makeThreadsRequest $ \response -> do
    putStrLn "i have respose wtf"
    void $ runMaybeT $ do
      body <- hoistMaybe response._responseBody >>= hoistMaybe . fromJSONValue @ThreadsResponseBody
      -- threadsResponseBody <- hoistMaybe $ fromJSONValue @ThreadsResponseBody body
      let threads = body.threads
      -- oldThreadsMap <- atomically $ readTVar (session ^. sessionThreads)
      let threadsMap = Map.fromList $ fmap (\(Thread id name) -> (id, DapThread id name True)) threads
      atomically $ writeTVar (session ^. sessionThreads) threadsMap

on :: MonadIO m => DapEnv -> Text -> EventCallback -> m ()
on env event callback =
  atomically $ modifyTVar' (env ^. eventCallbacks) (Map.insert event callback)

callEventCallback :: MonadIO m => DapEnv -> Session -> Text -> Event -> m ()
callEventCallback env session event value = do
  callbackMap <- readTVarIO (env ^. eventCallbacks)
  case Map.lookup event callbackMap of
    Just callback -> liftIO $ callback (value, session)
    Nothing -> pure ()

handleReverseRequest :: MonadIO m => DapEnv -> Text -> ReverseRequestCallback -> m ()
handleReverseRequest env command callback =
  atomically $ modifyTVar' (env ^. reverseRequestCallbacks) (Map.insert command callback)

callHandleReverseRequest :: MonadIO m => DapEnv -> Text -> Request -> m ()
callHandleReverseRequest env event value = do
  callbackMap <- readTVarIO (env ^. reverseRequestCallbacks)
  case Map.lookup event callbackMap of
    Just callback -> liftIO $ callback value
    Nothing -> pure ()
