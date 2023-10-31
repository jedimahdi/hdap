{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dap where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Dap.Protocol
import Dap.Request
import Dap.Session
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Lens.Micro
import Lens.Micro.Aeson
import Network.Simple.TCP qualified as TCP
import System.FilePath (takeDirectory)
import UnliftIO.Async
import UnliftIO.STM
import Utils

initialize :: MonadIO m => DapEnv -> m Session
initialize env = do
  session <- newSession env
  let sessionsVar = env ^. sessions
  atomically $ modifyTVar' sessionsVar (session :)

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
  sessionInitialize session args
  pure session
  where
    handleBody :: (MonadIO m, MonadThrow m) => DapEnv -> Session -> BS.ByteString -> m ()
    handleBody env session body = do
      let filePath = "/home/mahdi/tmp/app.js"
      js <- throwNothing $ Aeson.decode @Value (BS.fromStrict body)
      responseType <- throwNothing $ js ^? key "type" . _String
      when (responseType == "event" || responseType == "request") $ incSeq session

      when (responseType == "response") $ do
        printJSON js
        resSeq <- throwNothing $ js ^? key "request_seq" . _Number . _Integer
        callCallback session (fromIntegral resSeq) js

      when (responseType == "event") $ do
        event <- throwNothing $ js ^? key "event" . _String

        liftIO $ putStrLn $ "Session " <> show (session ^. sessionId)

        case parseEventType event of
          Just eventType -> do
            callAfterEventCallbacks env session eventType
            case eventType of
              DapEventOutput -> liftIO $ putStrLn "output event"
              DapEventLoadedSource -> liftIO $ putStrLn "loadedSource event"
              DapEventStopped -> do
                let mthreadId :: Maybe Int = fromIntegral <$> js ^? key "body" . key "threadId" . _Number . _Integer

                case mthreadId of
                  Nothing -> printJSON js
                  Just threadId -> do
                    printJSON js
                    atomically $ modifyTVar' (session ^. threads) (DapThread threadId True :)
                    pure ()

                pure ()
              _ -> printJSON js
          Nothing -> do
            pure ()
        -- liftIO $ print ("unknown event type" <> event)

        when (event == "initialized") $ do
          isAlreadyInit <- readTVarIO (session ^. isInitialized)

          unless isAlreadyInit $ do
            atomically $ writeTVar (session ^. isInitialized) True
            bps <- readTVarIO $ env ^. breakpoints
            sendRequest session (makeSetBreakpointsRequest filePath bps) \_ -> do
              sendRequest session makeSetExceptionBreakpointsRequest \_ -> do
                sendRequest session makeConfigurationDone \_ -> do
                  pure ()

      when (responseType == "request") $ do
        -- printJSON js
        resCommand <- throwNothing $ js ^? key "command" . _String
        when (resCommand == "startDebugging") $ do
          args <- throwNothing $ js ^? key "arguments" . key "configuration" . _Object
          requestId <- throwNothing $ js ^? key "seq" . _Number . _Integer
          session2 <- newSession env
          sendResponse session2 (fromIntegral requestId)
          sessionInitialize session2 args

    sessionInitialize :: MonadIO m => Session -> Object -> m ()
    sessionInitialize session args = do
      sendRequest session makeInitializeRequest \_ -> do
        -- TODO: save capabilities
        sendRequest session (makeLaunchRequest args) \_ -> do
          pure ()

    newSession :: MonadIO m => DapEnv -> m Session
    newSession env = do
      (socket, remoteAddr) <- TCP.connectSock "localhost" "8123"
      liftIO $ putStrLn $ "Connection established to " ++ show remoteAddr ++ " Socket: " <> show socket
      requestSeq <- newTVarIO 1
      callbacks <- newTVarIO mempty
      eventCs <- newTVarIO mempty
      threads <- newTVarIO []
      isInitialized <- newTVarIO False
      let nextSessionIdVar = env ^. nextSessionId
      sessionId <- atomically $ do
        i <- readTVar nextSessionIdVar
        writeTVar nextSessionIdVar (i + 1)
        pure i
      let session = Session sessionId requestSeq callbacks eventCs socket threads isInitialized
      _ <- liftIO $ async (createReadLoop socket (handleBody env session) (pure ()))
      pure session

setBreakpoint :: MonadIO m => DapEnv -> Int -> m ()
setBreakpoint env lineNumber = do
  atomically $ modifyTVar' (env ^. breakpoints) (lineNumber :)

continue :: _
continue = undefined

listBreakpoints :: _
listBreakpoints = undefined

clearBreakpoints :: _
clearBreakpoints = undefined

shutdown :: MonadIO m => DapEnv -> m ()
shutdown env = do
  m <- readTVarIO $ env ^. lastStopSession
  case m of
    Just s -> do
      TCP.closeSock (s ^. sessionSocket)
    Nothing -> liftIO $ putStrLn "wtf"
  pure ()
