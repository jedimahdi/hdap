{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dap.Session where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Dap.Parse
import Dap.Protocol
import Dap.Request
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
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.TH
import Network.Simple.TCP
import Network.Simple.TCP qualified as TCP
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.Async
import UnliftIO.STM
import Utils (printJSON, throwNothing)
import Prelude hiding (seq)

data DapEventType
  = DapEventOutput
  | DapEventStopped
  | DapEventLoadedSource
  | DapEventInitialized
  deriving (Eq, Ord)

parseEventType :: Text -> Maybe DapEventType
parseEventType "output" = Just DapEventOutput
parseEventType "stopped" = Just DapEventStopped
parseEventType "loadedSource" = Just DapEventLoadedSource
parseEventType "initialized" = Just DapEventInitialized
parseEventType _ = Nothing

type Callback = Value -> IO ()

data DapThread = DapThread
  { _threadId :: Int
  , _stopped :: Bool
  }

data Session = Session
  { _sessionId :: Int
  , _seqVar :: TVar RequestId
  , _messageCallbacks :: TVar (Map RequestId Callback)
  , _eventCallbacks :: TVar (Map DapEventType Callback)
  , _sessionSocket :: TCP.Socket
  , _threads :: TVar [DapThread]
  , _isInitialized :: TVar Bool
  }
makeLenses ''Session

data DapEnv = DapEnv
  { _sessions :: TVar [Session]
  , _afterEventCallbacks :: TVar (Map DapEventType [Session -> IO ()])
  , _lastStopSession :: TVar (Maybe Session)
  , _breakpoints :: TVar [Int]
  , _nextSessionId :: TVar Int
  }
makeLenses ''DapEnv

newEnv :: MonadIO m => m DapEnv
newEnv = do
  sessions <- newTVarIO []
  lastStopSession <- newTVarIO Nothing
  afterEventCallbacks <- newTVarIO $ Map.fromList [(DapEventStopped, [updateStoppedSession lastStopSession])]
  bps <- newTVarIO []
  nextSessionIdVar <- newTVarIO 1
  pure $ DapEnv sessions afterEventCallbacks lastStopSession bps nextSessionIdVar
 where
  updateStoppedSession :: TVar (Maybe Session) -> Session -> IO ()
  updateStoppedSession lastStopSessionVar session = do
    putStrLn "sheeesh"
    atomically $ writeTVar lastStopSessionVar (Just session)

addAfterEventCallback :: MonadIO m => DapEnv -> DapEventType -> (Session -> IO ()) -> m ()
addAfterEventCallback env eventType callback =
  atomically $ modifyTVar' (env ^. afterEventCallbacks) (Map.insertWith (<>) eventType [callback])

callAfterEventCallbacks :: MonadIO m => DapEnv -> Session -> DapEventType -> m ()
callAfterEventCallbacks env session eventType = do
  callMap <- readTVarIO (env ^. afterEventCallbacks)
  case Map.lookup eventType callMap of
    Just cs -> liftIO $ do
      forM_ cs $ \c -> do
        c session
    Nothing -> pure ()

addEventCallback :: MonadIO m => Session -> DapEventType -> Callback -> m ()
addEventCallback session eventType callback =
  atomically $ modifyTVar' (session ^. eventCallbacks) (Map.insert eventType callback)

addCallback :: MonadIO m => Session -> RequestId -> Callback -> m ()
addCallback session requestId callback =
  atomically $ modifyTVar' (session ^. messageCallbacks) (Map.insert requestId callback)

callCallback :: MonadIO m => Session -> RequestId -> Value -> m ()
callCallback session requestId json = do
  callMap <- readTVarIO (session ^. messageCallbacks)
  case Map.lookup requestId callMap of
    Just callback -> liftIO $ callback json
    Nothing -> pure ()

incSeq :: MonadIO m => Session -> m ()
incSeq session = atomically $ modifyTVar' (session ^. seqVar) (+ 1)

sendRequest :: MonadIO m => Session -> (RequestId -> Request) -> Callback -> m ()
sendRequest session mkRequest callback = do
  let seqTVar = session ^. seqVar
  requestId <- atomically $ do
    n <- readTVar seqTVar
    writeTVar seqTVar (n + 1)
    pure n
  let socket = session ^. sessionSocket
  addCallback session requestId callback
  let request = mkRequest requestId
  printJSON (Aeson.toJSON request)
  send socket (addContentLength (Aeson.encode request))

sendResponse :: MonadIO m => Session -> RequestId -> m ()
sendResponse session requestId = do
  let seqTVar = session ^. seqVar
  responseId <- atomically $ do
    n <- readTVar seqTVar
    writeTVar seqTVar (n + 1)
    pure n
  let socket = session ^. sessionSocket
  let body =
        KeyMap.fromList
          [ ("seq", Number (fromIntegral responseId))
          , ("request_seq", Number (fromIntegral requestId))
          , ("type", "response")
          , ("command", String "startDebugging")
          , ("success", Bool True)
          ]
  send socket (addContentLength (Aeson.encode body))

addContentLength :: BS.Char8.ByteString -> BS.ByteString
addContentLength s =
  BS.toStrict $
    "Content-Length: "
      <> BS.Char8.pack (show (BSL.length s))
      <> "\r\n\r\n"
      <> s
