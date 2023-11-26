{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use <$>" #-}
module Cli where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.Trans.Maybe
import Dap
import Dap.Env
import Dap.Protocol
import Dap.Request
import Dap.Session
import Dap.Types
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
import Data.Text.IO qualified as TIO
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream (Stream)
import Network.WebSockets.Stream qualified as Stream
import System.FilePath
import Text.Pretty.Simple (pPrint, pPrintString)
import UnliftIO (MonadIO, async, liftIO, wait, waitCatch)
import UnliftIO.STM
import Utils

data MyException = MyException
  deriving (Show)

instance Exception MyException

myBracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
myBracket before after thing = do
  mask \restore -> do
    a <- before
    c <- restore (thing a) `onException` after a
    after a
    pure c

cli :: IO ()
cli = do
  let filePath = "/home/mahdi/tmp/app.js"
  let args =
        object
          [ "request" .= String "launch"
          , "type" .= String "pwa-node"
          , "name" .= String "Launch file"
          , "program" .= String filePath
          , "runtimeExecutable" .= String "node"
          , "cwd" .= String (T.pack (takeDirectory (T.unpack filePath)))
          ]
  session <- initialize args
  setBreakpoint session filePath [2]
  (session', newArgs) <- startDebugging session
  go session' filePath newArgs

  getLine
  pure ()
  where
    go session filePath args = do
      setBreakpoint session filePath [2]
      waitForStoppedEvent <- expectEvent "stopped" session
      let configuration = args.configuration
      let requestType = String args.request
      let fullConfig = KeyMap.insert "request" requestType configuration
      launchReqId <- sendRequest (makeLaunchRequest (Object fullConfig)) session
      stopped <- waitForStoppedEvent
      threadsReqId <- sendRequest makeThreadsRequest session
      threadsBody <- waitForResposne @ThreadsResponseBody threadsReqId session
      let thread = head threadsBody.threads
      stackTraceReqId <- sendRequest (makeStackTraceRequest thread.id) session
      stackTraceBody <- waitForResposne @StackTraceResponseBody stackTraceReqId session
      let frame = head stackTraceBody.stackFrames
      scopesReqId <- sendRequest (makeScopesRequest frame.id) session
      scopesBody <- waitForResposne @ScopesResponseBody scopesReqId session
      let scope = head scopesBody.scopes
      variablesReqId <- sendRequest (makeVariablesRequest scope.variablesReference) session
      variablesBody <- waitForResposne @VariablesResponseBody variablesReqId session
      pPrint variablesBody.variables

    f filePath session = do
      setBreakpointReqId <- sendRequest (makeSetBreakpointsRequest filePath [2]) session
      setExceptionBreakpointId <- sendRequest makeSetExceptionBreakpointsRequest session
      configureDoneReqId <- sendRequest makeConfigurationDone session
      waitForResposne @() setBreakpointReqId session
      waitForResposne @() setExceptionBreakpointId session
      waitForResposne @() configureDoneReqId session

cli2 :: IO ()
cli2 = do
  let filePath = "/home/mahdi/tmp/app.js"
  let args =
        object
          [ "request" .= String "launch"
          , "type" .= String "pwa-node"
          , "name" .= String "Launch file"
          , "program" .= String filePath
          , "runtimeExecutable" .= String "node"
          , "cwd" .= String (T.pack (takeDirectory (T.unpack filePath)))
          ]
  env <- newEnv
  -- initialize env args
  -- loop env
  pure ()
  where

-- loop env = do
--   getLine >>= \case
--     "n" -> do
--       pPrintString "Starting Next..."
--       next env (const $ pure ())
--
--       after env "stopped" "next" \(event, session) -> do
--         printVariables session
--         pure ()
--       pPrintString "End of Next..."
--       loop env
--     "l" -> do
--       session <- readTVarIO env.session >>= throwNothing
--       thread <- readTVarIO session.threads >>= throwNothing . listToMaybe . Map.elems
--       sendRequest session (makeStackTraceRequest thread.id) handleStackTraceRes
--       loop env
--     "q" -> pure ()
--     _ -> loop env

-- handleStackTraceRes :: Session -> StackTraceResponseBody -> HandlerM ()
-- handleStackTraceRes session body = do
--   currentFrame <- throwNothing $ getTopFrame body.stackFrames
--   source <- throwNothing currentFrame.source
--   -- pPrint currentFrame
--   let line = currentFrame.line
--   sendRequest session (makeSourceRequest source) (handleSourceRes line)
--   pure ()
--
-- handleSourceRes :: Int -> Session -> SourceResponseBody -> HandlerM ()
-- handleSourceRes n session body = do
--   let aroundContent = T.unlines $ take 3 $ drop (n - 2) $ T.lines body.content
--   -- pPrint body
--   liftIO $ TIO.putStrLn aroundContent
--   pure ()

-- printVariables :: Session -> IO ()
-- printVariables session = do
--   liftIO $ threadDelay 1_000_000
--   thread <- readTVarIO session.threads >>= throwNothing . listToMaybe . Map.elems
--   sendRequest session (makeStackTraceRequest thread.id) handleStackTraceRes
--   where
--     handleStackTraceRes :: Session -> StackTraceResponseBody -> HandlerM ()
--     handleStackTraceRes session body = do
--       currentFrame <- throwNothing $ getTopFrame body.stackFrames
--       sendRequest session (makeScopesRequest currentFrame.id) hanldeScopesRes
--
--     hanldeScopesRes :: Session -> ScopesResponseBody -> HandlerM ()
--     hanldeScopesRes session body = do
--       scope <- throwNothing $ listToMaybe body.scopes
--       -- liftIO $ print scope
--       sendRequest session (makeVariablesRequest scope.variablesReference) handleVariablesRes
--
--     handleVariablesRes :: Session -> VariablesResponseBody -> HandlerM ()
--     handleVariablesRes _ body = do
--       forM_ body.variables \variable -> do
--         let variableType = case variable._type of
--               Nothing -> ""
--               Just t -> ": " <> t
--         liftIO $ TIO.putStrLn $ variable.name <> variableType <> " = " <> T.take 15 variable.value
