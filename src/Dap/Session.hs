module Dap.Session where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent.STM (STM, atomically, throwSTM)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Exception (AssertionFailed (..), throwIO)
import Control.Monad (forM_, join, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, execState, runStateT)
import Dap.Event
import Dap.Protocol
import Dap.Request
import Dap.Response
import Dap.Types
import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import Data.Foldable (traverse_)
import Data.IORef
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
import Prettyprinter
import System.FilePath (takeDirectory, takeFileName)
import Text.Pretty.Simple
import Utils (addContentLength, fromJSONValue, printJSON, throwNothing)
import Prelude hiding (seq)

data DapClientLog
  = Starting
  | SendMsg Request
  | ReceiveMsg MsgIn
  deriving (Show)

instance Pretty DapClientLog where
  pretty Starting = "Starting server"
  pretty (SendMsg msg) = "---> " <> pretty msg
  pretty (ReceiveMsg msg) = "<--- " <> pretty msg

type ResponseCallback = Response -> IO ()

data ThreadState = ThreadState
  { id :: Int
  , name :: Text
  , stopped :: Bool
  }

data Session = Session
  { nextRequestId :: !(TVar RequestId)
  , messageCallbacks :: !(TVar (Map RequestId (Maybe ResponseCallback)))
  , socketRef :: !(IORef TCP.Socket)
  , threads :: !(TVar (Map Int ThreadState))
  , isInitialized :: !(TVar Bool)
  , isLaunched :: !(IORef Bool)
  , streamRef :: !(IORef Stream)
  , eventsChan :: !(TQueue Event)
  , reverseRequestsChan :: !(TQueue Request)
  , clientOut :: !(TChan MsgOut)
  , responseMap :: !(TVar (Map RequestId Response))
  , events :: !(TChan Event)
  , reverseRequests :: !(TChan Request)
  , logger :: !(LogAction IO (WithSeverity DapClientLog))
  , args :: !(IORef Value)
  }

newSession :: LogAction IO (WithSeverity DapClientLog) -> TCP.Socket -> IO Session
newSession logger socket = do
  stream <- Stream.makeSocketStream socket
  messageCallbacks <- newTVarIO mempty
  nextRequestId <- newTVarIO 1
  threads <- newTVarIO mempty
  isInitialized <- newTVarIO False
  msgOutChan <- atomically newTQueue
  msgInChan <- atomically newTQueue
  responsesChan <- atomically newTQueue
  eventsChan <- atomically newTQueue
  reverseRequestsChan <- atomically newTQueue
  reverseRequests <- newTChanIO
  events <- newTChanIO
  clientOut <- newTChanIO
  requestToSend <- newEmptyTMVarIO
  responseMap <- newTVarIO mempty
  socketRef <- newIORef socket
  streamRef <- newIORef stream
  isLaunched <- newIORef False
  args <- newIORef Null
  pure $ Session {..}

sendRequest :: (RequestId -> Request) -> Session -> IO RequestId
sendRequest mkRequest session = do
  let nextRequestIdVar = session.nextRequestId
  requestId <- atomically $ do
    n <- readTVar nextRequestIdVar
    writeTVar nextRequestIdVar (n + 1)
    pure n
  let request = mkRequest requestId
  -- session.logger <& SendMsg request `WithSeverity` Debug
  atomically $ writeTChan session.clientOut request
  pure requestId

waitForResposne :: FromJSON body => RequestId -> Session -> IO body
waitForResposne requestId session = do
  res <- atomically $ do
    resMap <- readTVar session.responseMap
    maybe STM.retry pure (Map.lookup requestId resMap)
  case fromJSONValue =<< res.body of
    Nothing -> do
      liftIO $ putStrLn "Error parsing response"
      throwIO $ AssertionFailed "waitForResposne: Error"
    Just body -> do
      pure body

expectEvent :: Text -> Session -> IO (IO Event)
expectEvent eventType session = do
  chan <- atomically $ dupTChan session.events
  pure $ do
    let loop = do
          e <- atomically $ readTChan chan
          -- pPrintString $ "From expectEvent of " <> show eventType <> ": " <> show e
          if e.event == eventType
            then pure e
            else loop
    loop

expectReverseRequest :: Text -> Session -> IO (IO Request)
expectReverseRequest reqType session = do
  chan <- atomically $ dupTChan session.reverseRequests
  pure $ do
    let loop = do
          e <- atomically $ readTChan chan
          pPrintString $ "From expectReverseRequest of " <> show reqType <> ": " <> show e
          if e.command == reqType
            then pure e
            else loop
    loop

-- getThread :: MonadIO m => Session -> Int -> m (Maybe ThreadState)
-- getThread session threadId = Map.lookup threadId <$> readTVarIO session.threads
--
-- addCallback :: MonadIO m => Session -> RequestId -> ResponseCallback -> m ()
-- addCallback session requestId callback =
--   atomically $ modifyTVar' session.messageCallbacks (Map.insert requestId (Just callback))
--
-- addCallback' :: MonadIO m => Session -> RequestId -> ResponseCallback -> m ()
-- addCallback' session requestId callback =
--   atomically $ modifyTVar' session.messageCallbacks (Map.insert requestId (Just callback))
--
-- callCallback :: MonadIO m => Session -> RequestId -> Response -> m ()
-- callCallback session requestId value = do
--   let messageCallbacksVar = session.messageCallbacks
--   callbackMap <- atomically $ do
--     originalMap <- readTVar messageCallbacksVar
--     writeTVar messageCallbacksVar (Map.insert requestId Nothing originalMap)
--     pure originalMap
--   case Map.lookup requestId callbackMap of
--     Just (Just callback) -> do
--       liftIO $ callback value
--     _ -> pure ()

-- addMsgOut :: Session -> MsgOut -> IO ()
-- addMsgOut session msg = atomically $ writeTQueue session.msgOutChan msg

-- sendRequest2 :: (MonadIO m, FromJSON body) => (RequestId -> Request) -> Session -> m body
-- sendRequest2 mkRequest session = do
--   let nextRequestIdVar = session.nextRequestId
--   requestId <- atomically $ do
--     n <- readTVar nextRequestIdVar
--     writeTVar nextRequestIdVar (n + 1)
--     pure n
--   let request = mkRequest requestId
--   -- printJSON (Aeson.toJSON request)
--   atomically $ do
--     putTMVar session.requestToSend request
--
--   res <- atomically $ readTQueue session.responsesChan
--
--   when (res.requestId /= requestId) $ throwIO (AssertionFailed "slkadj")
--   case fromJSONValue =<< res.body of
--     Nothing -> do
--       liftIO $ putStrLn "Error parsing response"
--       throwIO $ AssertionFailed "salkdj"
--     Just body -> do
--       pure body

-- sendRequest_ :: Session -> (RequestId -> Request) -> ResponseCallback -> IO ()
-- sendRequest_ session mkRequest callback = do
--   let nextRequestIdVar = session.nextRequestId
--   requestId <- atomically $ do
--     n <- readTVar nextRequestIdVar
--     writeTVar nextRequestIdVar (n + 1)
--     pure n
--   addCallback session requestId callback
--   let request = mkRequest requestId
--   -- printJSON (Aeson.toJSON request)
--   addMsgOut session request
--
-- sendRequest :: FromJSON a => Session -> (RequestId -> Request) -> (Session -> a -> MaybeT IO ()) -> IO ()
-- sendRequest session mkRequest handler = do
--   sendRequest_ session mkRequest (mkRespponseCallback (handler session))
--   where
--     mkRespponseCallback :: FromJSON a => (a -> MaybeT IO ()) -> Response -> IO ()
--     mkRespponseCallback callback response = do
--       _ <- runMaybeT $ do
--         rawBody <- hoistMaybe response.body
--         body <- hoistMaybe $ fromJSONValue rawBody
--         callback body
--       pure ()
