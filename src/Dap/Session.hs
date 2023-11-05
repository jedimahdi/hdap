{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dap.Session where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Dap.Event
import Dap.Protocol
import Dap.Request
import Dap.Response
import Dap.Types
import Data.Aeson (ToJSON, Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.Simple.TCP
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream (Stream)
import Network.WebSockets.Stream qualified as Stream
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.Async
import UnliftIO.STM
import Utils (addContentLength, printJSON, throwNothing)
import Prelude hiding (seq)

type ResponseCallback = Response -> IO ()

data ThreadState = ThreadState
  { id :: Int
  , name :: Text
  , stopped :: Bool
  }

data Session = Session
  { id :: !Int
  , nextRequestId :: !(TVar RequestId)
  , messageCallbacks :: !(TVar (Map RequestId (Maybe ResponseCallback)))
  , socket :: !TCP.Socket
  , threads :: !(TVar (Map Int ThreadState))
  , isInitialized :: !(TVar Bool)
  , msgOutChan :: !(TQueue MsgOut)
  , msgInChan :: !(TQueue MsgIn)
  , stream :: !Stream
  }

instance Eq Session where
  s1 == s2 = s1.id == s2.id

instance Show Session where
  show s = "<Session " ++ show s.id ++ ">"

newSession :: MonadIO m => Int -> TCP.Socket -> m Session
newSession sessionId socket = do
  stream <- liftIO $ Stream.makeSocketStream socket
  messageCallbacks <- newTVarIO mempty
  nextRequestId <- newTVarIO 1
  -- stoppedThreadId <- newTVarIO Nothing
  threads <- newTVarIO mempty
  isInitialized <- newTVarIO False
  msgOut <- atomically newTQueue
  msgIn <- atomically newTQueue
  pure
    $ Session
      { id = sessionId
      , nextRequestId = nextRequestId
      , messageCallbacks = messageCallbacks
      , socket = socket
      , threads = threads
      , isInitialized = isInitialized
      , msgOutChan = msgOut
      , msgInChan = msgIn
      , stream = stream
      }

addCallback :: MonadIO m => Session -> RequestId -> ResponseCallback -> m ()
addCallback session requestId callback =
  atomically $ modifyTVar' session.messageCallbacks (Map.insert requestId (Just callback))

callCallback :: MonadIO m => Session -> RequestId -> Response -> m ()
callCallback session requestId value = do
  let messageCallbacksVar = session.messageCallbacks
  callbackMap <- atomically $ do
    originalMap <- readTVar messageCallbacksVar
    writeTVar messageCallbacksVar (Map.insert requestId Nothing originalMap)
    pure originalMap
  case Map.lookup requestId callbackMap of
    Just (Just callback) -> do
      liftIO $ callback value
    _ -> pure ()

addMsgOut :: MonadIO m => Session -> MsgOut -> m ()
addMsgOut session msg = atomically $ writeTQueue session.msgOutChan msg

sendRequest :: MonadIO m => Session -> (RequestId -> Request) -> ResponseCallback -> m ()
sendRequest session mkRequest callback = do
  let nextRequestIdVar = session.nextRequestId
  requestId <- atomically $ do
    n <- readTVar nextRequestIdVar
    writeTVar nextRequestIdVar (n + 1)
    pure n
  addCallback session requestId callback
  let request = mkRequest requestId
  -- printJSON (Aeson.toJSON request)
  addMsgOut session request

-- sendResponse :: MonadIO m => Session -> RequestId -> m ()
-- sendResponse session requestId = do
--   let seqTVar = session ^. seqVar
--   responseId <- atomically $ do
--     n <- readTVar seqTVar
--     writeTVar seqTVar (n + 1)
--     pure n
--   let socket = session ^. sessionSocket
--   let body =
--         KeyMap.fromList
--           [ ("seq", Number (fromIntegral responseId))
--           , ("request_seq", Number (fromIntegral requestId))
--           , ("type", "response")
--           , ("command", String "startDebugging")
--           , ("success", Bool True)
--           ]
--   send socket (addContentLength (Aeson.encode body))

-- addEventCallback :: MonadIO m => Session -> DapEventType -> Callback -> m ()
-- addEventCallback session eventType callback =
--   atomically $ modifyTVar' (session ^. eventCallbacks) (Map.insert eventType callback)
--
-- callCallback :: MonadIO m => Session -> RequestId -> Value -> m ()
-- callCallback session requestId json = do
--   callMap <- readTVarIO (session ^. messageCallbacks)
--   case Map.lookup requestId callMap of
--     Just callback -> liftIO $ callback json
--     Nothing -> pure ()
--
-- incSeq :: MonadIO m => Session -> m ()
-- incSeq session = atomically $ modifyTVar' (session ^. seqVar) (+ 1)
--
