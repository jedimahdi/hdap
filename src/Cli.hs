{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Use const" #-}

module Cli where

import Control.Monad (forM_, forever, unless, void, when)
import Dap
import Dap.Protocol
import Dap.Request
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL.Char8
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.TH
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream (Stream)
import Network.WebSockets.Stream qualified as Stream
import System.FilePath
import Text.Pretty.Simple (pPrint)
import UnliftIO (MonadIO, async, liftIO)
import UnliftIO.STM
import Utils

data Session = Session
  { _sessionStream :: !Stream
  , _sessionSocket :: !TCP.Socket
  , _requestCallbacks :: !(TVar (Map RequestId (IO ())))
  , _nextRequestId :: !(TVar RequestId)
  , _stoppedThreadId :: !(TVar (Maybe Int))
  -- , _requests :: !(TChan Request)
  }
makeLenses ''Session

setRequestCallback :: MonadIO m => Session -> RequestId -> IO () -> m ()
setRequestCallback session requestId callback =
  atomically $ modifyTVar' (session ^. requestCallbacks) (Map.insert requestId callback)

callRequestCallback :: MonadIO m => Session -> RequestId -> m ()
callRequestCallback session requestId = do
  maybeCallback <- atomically $ do
    mr <- Map.lookup requestId <$> readTVar (session ^. requestCallbacks)
    modifyTVar' (session ^. requestCallbacks) (Map.delete requestId)
    pure mr
  forM_ maybeCallback liftIO

newSession :: MonadIO m => TCP.Socket -> Stream -> m Session
newSession socket stream = do
  requestCallbacks <- newTVarIO mempty
  nextRequestId <- newTVarIO 1
  stoppedThreadId <- newTVarIO Nothing
  pure $ Session stream socket requestCallbacks nextRequestId stoppedThreadId

run :: Object -> IO ()
run args = do
  (socket, remoteAddr) <- TCP.connectSock "localhost" "8123"
  putStrLn $ "Connection established to " ++ show remoteAddr ++ " Socket: " <> show socket
  stream <- Stream.makeSocketStream socket
  session <- newSession socket stream
  void $ async $ readLoop stream (handleBody session) (pure ())
  send session makeInitializeRequest $ do
    pure ()
  send session (makeLaunchRequest args) $ do
    pure ()
  _ <- getLine
  pure ()

handleBody :: Session -> Aeson.Value -> IO ()
handleBody session msg = do
  msgType <- throwNothing $ msg ^? key "type" . _String

  when (msgType == "response") handleResponseMsg
  when (msgType == "event") handleEventMsg
  when (msgType == "request") handleRequestMsg
  where
    handleResponseMsg :: IO ()
    handleResponseMsg = do
      printJSON msg
      -- command <- throwNothing $ msg ^? key "command" . _String
      requestId <- throwNothing $ msg ^? key "request_seq" . _Number . _Integral
      callRequestCallback session requestId

    handleEventMsg :: IO ()
    handleEventMsg = do
      let filePath = "/home/mahdi/tmp/app.js"
      event <- throwNothing $ msg ^? key "event" . _String
      unless (event == "loadedSource" || event == "output") $ printJSON msg
      case event of
        "initialized" ->
          send session (makeSetBreakpointsRequest filePath [2]) $ do
            send session makeSetExceptionBreakpointsRequest $ do
              send session makeConfigurationDone $ do
                pure ()
        "thread" -> do
          let stoppedThreadIdVar = session ^. stoppedThreadId
          threadId <- throwNothing $ msg ^? key "body" . key "threadId" . _Number . _Integral
          atomically $ writeTVar stoppedThreadIdVar (Just threadId)
        "stopped" -> do
          let stoppedThreadIdVar = session ^. stoppedThreadId
          readTVarIO stoppedThreadIdVar >>= \case
            Just threadId -> do
              pure ()
            Nothing -> pure ()
        _ -> pure ()
      pure ()

    handleRequestMsg :: IO ()
    handleRequestMsg = do
      printJSON msg
      command <- throwNothing $ msg ^? key "command" . _String
      case command of
        "startDebugging" -> do
          configuration <- throwNothing $ msg ^? key "arguments" . key "configuration" . _Object
          requestType <- throwNothing $ msg ^? key "arguments" . key "request"
          let fullConfig = KeyMap.insert "request" requestType configuration
          run fullConfig
        _ -> pure ()

send :: MonadIO m => Session -> (RequestId -> Request) -> IO () -> m ()
send session mkRequest callback = do
  -- printJSON (Aeson.toJSON request)
  let nextRequestIdVar = session ^. nextRequestId
  let stream = session ^. sessionStream
  requestId <- atomically $ do
    n <- readTVar nextRequestIdVar
    writeTVar nextRequestIdVar (n + 1)
    pure n
  setRequestCallback session requestId callback
  liftIO $ Stream.write stream (addContentLength (Aeson.encode (mkRequest requestId)))

addContentLength :: BSL.ByteString -> BSL.ByteString
addContentLength s =
  "Content-Length: "
    <> BSL.Char8.pack (show (BSL.length s))
    <> "\r\n\r\n"
    <> s

cli :: IO ()
cli = do
  let filePath = "/home/mahdi/tmp/app.js"
  let args =
        KeyMap.fromList
          [ ("request", String "launch")
          , ("type", String "pwa-node")
          , ("name", String "Launch file")
          , ("program", String filePath)
          , ("runtimeExecutable", String "node")
          , ("cwd", String (T.pack (takeDirectory (T.unpack filePath))))
          ]
  run args
  pure ()

-- cli :: IO ()
-- cli = do
--   env <- newEnv
--
--   void $ forever $ do
--     line <- getLine
--     case line of
--       ('b' : xs) | all isDigit xs -> setBreakpoint env (read xs)
--       "i" -> do
--         void $ initialize env
--       -- addAfterEventCallback env DapEventInitialized (\_ -> putStrLn "Initialized")
--       -- addAfterEventCallback env DapEventStopped (\_ -> putStrLn "Stopped")
--
--       "t" -> do
--         msession <- readTVarIO $ env ^. lastStopSession
--         case msession of
--           Nothing -> putStrLn "No Stopped session..."
--           Just session -> do
--             print (session ^. sessionId)
--             sendRequest session makeThreadsRequest \res -> do
--               print $ prettyJSON res
--       "s" -> do
--         msession <- readTVarIO $ env ^. lastStopSession
--         case msession of
--           Nothing -> putStrLn "No Stopped session..."
--           Just session -> do
--             print (session ^. sessionId)
--             t <- listToMaybe <$> readTVarIO (session ^. threads)
--             case t of
--               Nothing -> putStrLn "No Threads..."
--               Just tt -> do
--                 sendRequest session (makeStackTraceRequest (_threadId tt)) \res -> do
--                   print $ prettyJSON res
--       _ -> pure ()
--
--   shutdown env
--   pure ()
