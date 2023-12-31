module Dap.Env where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Dap.Event
import Dap.Protocol
import Dap.Request
import Dap.Session (Session)
import Dap.Types
import Data.Aeson (ToJSON, Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.Simple.TCP
import Network.Simple.TCP qualified as TCP
import System.FilePath (takeDirectory, takeFileName)
import UnliftIO.Async
import UnliftIO.STM
import Utils (printJSON, throwNothing)

type EventCallback = (Event, Session) -> IO ()
type ReverseRequestCallback = Request -> IO ()

data DapEnv = DapEnv
  { -- , _afterEventCallbacks :: TVar (Map DapEventType [Session -> IO ()])
    session :: !(TVar (Maybe Session))
  , breakpoints :: !(TVar [Breakpoint])
  , nextSessionId :: !(TVar Int)
  , eventCallbacks :: !(TVar (Map Text EventCallback))
  , reverseRequestCallbacks :: !(TVar (Map Text ReverseRequestCallback))
  , afterEventCallbacks :: !(TVar (Map (Text, Text) [EventCallback]))
  }

newEnv :: MonadIO m => m DapEnv
newEnv = do
  envSession <- newTVarIO Nothing
  eventCallbacks <- newTVarIO mempty
  reverseRequestCallbacks <- newTVarIO mempty
  afterEventCallbacks <- newTVarIO mempty
  bps <- newTVarIO []
  nextSessionIdVar <- newTVarIO 1
  pure $ DapEnv envSession bps nextSessionIdVar eventCallbacks reverseRequestCallbacks afterEventCallbacks

getNextSessionId :: MonadIO m => DapEnv -> m Int
getNextSessionId env = do
  let nextSessionIdVar = env.nextSessionId
  atomically $ do
    i <- readTVar nextSessionIdVar
    writeTVar nextSessionIdVar (i + 1)
    pure i

after :: MonadIO m => DapEnv -> Text -> Text -> EventCallback -> m ()
after env eventType name callback =
  atomically $ modifyTVar' env.afterEventCallbacks (Map.insertWith (<>) (eventType, name) [callback])

callAfterEventCallbacks :: MonadIO m => DapEnv -> Event -> Session -> Text -> m ()
callAfterEventCallbacks env event session eventType = do
  callMap <- readTVarIO env.afterEventCallbacks
  let cs = concatMap snd $ List.filter (\((e, _), _) -> e == eventType) $ Map.assocs callMap
  liftIO $ forM_ cs $ \c -> do
    c (event, session)
