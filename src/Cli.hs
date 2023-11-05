{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Use const" #-}

module Cli where

import Control.Monad (forM_, forever, unless, void, when)
import Dap
import Dap.Env
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
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream (Stream)
import Network.WebSockets.Stream qualified as Stream
import System.FilePath
import Text.Pretty.Simple (pPrint)
import UnliftIO (MonadIO, async, liftIO)
import UnliftIO.STM
import Utils

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
  _ <- getLine
  pure ()
