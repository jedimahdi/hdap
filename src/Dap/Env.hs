module Dap.Env where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (when)
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
  }

newEnv :: MonadIO m => m DapEnv
newEnv = do
  envSession <- newTVarIO Nothing
  eventCallbacks <- newTVarIO mempty
  reverseRequestCallbacks <- newTVarIO mempty
  bps <- newTVarIO []
  nextSessionIdVar <- newTVarIO 1
  pure $ DapEnv envSession bps nextSessionIdVar eventCallbacks reverseRequestCallbacks

getNextSessionId :: MonadIO m => DapEnv -> m Int
getNextSessionId env = do
  let nextSessionIdVar = env.nextSessionId
  atomically $ do
    i <- readTVar nextSessionIdVar
    writeTVar nextSessionIdVar (i + 1)
    pure i

-- addAfterEventCallback :: MonadIO m => DapEnv -> DapEventType -> (Session -> IO ()) -> m ()
-- addAfterEventCallback env eventType callback =
--   atomically $ modifyTVar' (env ^. afterEventCallbacks) (Map.insertWith (<>) eventType [callback])
--
-- callAfterEventCallbacks :: MonadIO m => DapEnv -> Session -> DapEventType -> m ()
-- callAfterEventCallbacks env session eventType = do
--   callMap <- readTVarIO (env ^. afterEventCallbacks)
--   case Map.lookup eventType callMap of
--     Just cs -> liftIO $ do
--       forM_ cs $ \c -> do
--         c session
--     Nothing -> pure ()
