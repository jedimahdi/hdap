module Cli where

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
import UnliftIO (MonadIO, async, liftIO)
import UnliftIO.STM
import Utils
import Control.Concurrent

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
  env <- newEnv
  initialize env args
  loop env
  pure ()
  where
    loop env = do
      getLine >>= \case
        "n" -> do
          pPrintString "Starting Next..."
          next env (const $ pure ())

          after env "stopped" "next" \(event, session) -> do
            printVariables session
            pure ()
          pPrintString "End of Next..."
          loop env
        "l" -> do
          session <- readTVarIO env.session >>= throwNothing
          thread <- readTVarIO session.threads >>= throwNothing . listToMaybe . Map.elems
          sendRequest session (makeStackTraceRequest thread.id) handleStackTraceRes
          loop env
        "q" -> pure ()
        _ -> loop env

    handleStackTraceRes :: Session -> StackTraceResponseBody -> HandlerM ()
    handleStackTraceRes session body = do
      currentFrame <- throwNothing $ getTopFrame body.stackFrames
      source <- throwNothing currentFrame.source
      -- pPrint currentFrame
      let line = currentFrame.line
      sendRequest session (makeSourceRequest source) (handleSourceRes line)
      pure ()

    handleSourceRes :: Int -> Session -> SourceResponseBody -> HandlerM ()
    handleSourceRes n session body = do
      let aroundContent = T.unlines $ take 3 $ drop (n - 2) $ T.lines body.content
      -- pPrint body
      liftIO $ TIO.putStrLn aroundContent
      pure ()

printVariables :: Session -> IO ()
printVariables session = do
  liftIO $ threadDelay 1_000_000
  thread <- readTVarIO session.threads >>= throwNothing . listToMaybe . Map.elems
  sendRequest session (makeStackTraceRequest thread.id) handleStackTraceRes
  where
    handleStackTraceRes :: Session -> StackTraceResponseBody -> HandlerM ()
    handleStackTraceRes session body = do
      currentFrame <- throwNothing $ getTopFrame body.stackFrames
      sendRequest session (makeScopesRequest currentFrame.id) hanldeScopesRes

    hanldeScopesRes :: Session -> ScopesResponseBody -> HandlerM ()
    hanldeScopesRes session body = do
      scope <- throwNothing $ listToMaybe body.scopes
      -- liftIO $ print scope
      sendRequest session (makeVariablesRequest scope.variablesReference) handleVariablesRes

    handleVariablesRes :: Session -> VariablesResponseBody -> HandlerM ()
    handleVariablesRes _ body = do
      forM_ body.variables \variable -> do
        let variableType = case variable._type of
              Nothing -> ""
              Just t -> ": " <> t
        liftIO $ TIO.putStrLn $ variable.name <> variableType <> " = " <> T.take 15 variable.value
