{-# LANGUAGE PartialTypeSignatures #-}

module Dap.Protocol where

import Control.Exception.Safe
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe
import Dap.Parse
import Data.Aeson (Value)
import Data.Aeson.Parser (json')
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as Parser
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as ByteString.Short
import Data.Char (chr)
import Data.Function ((&))
import Data.List qualified as List
import Data.Word (Word8)
import Data.Word8 (_colon, _cr, _lf, _space, _tab)
import Data.Word8 qualified as Word8
import Network.Simple.TCP (Socket)
import Network.Simple.TCP qualified as TCP
import Network.WebSockets.Stream as Stream
import System.IO (Handle, IOMode (..), openFile, stdin, stdout)
import Utils (throwLeft)

skipSpace :: Parser ()
skipSpace = Parser.skipWhile $ \w -> w == _space || w == _tab

headerParser :: Parser (ByteString, ByteString)
headerParser = do
  key <- Parser.many1' $ Parser.satisfy (\w -> w /= _colon && w /= _cr && w /= _lf)
  skipSpace
  _ <- Parser.word8 _colon
  skipSpace
  value <- Parser.many1' $ Parser.satisfy (\w -> w /= _cr && w /= _lf)
  pure (BS.pack key, BS.pack value)

headersParser :: Parser [(ByteString, ByteString)]
headersParser = Parser.sepBy' headerParser (Parser.string "\r\n")

bodyParser :: Parser Value
bodyParser = json'

dapProtocolParser :: Parser Value
dapProtocolParser = do
  _ <- headersParser
  _ <- Parser.string "\r\n\r\n"
  bodyParser

readLoop :: (MonadIO m, MonadThrow m) => Stream.Stream -> (Value -> m a) -> m b -> m b
readLoop stream handleBody onNoChunk = do
  loop
 where
  loop = do
    md <- liftIO $ parse stream dapProtocolParser
    case md of
      Nothing -> onNoChunk
      Just value -> do
        void $ handleBody value
        loop

recieve :: (MonadIO m, MonadThrow m) => Socket -> BS.ByteString -> m (Maybe (BS.ByteString, BS.ByteString))
recieve socket rem = runMaybeT $ do
  x <- MaybeT $ TCP.recv socket chunkSize
  y <- throwLeft $ parseChunkLoop (Initial (rem <> x))
  loop y
 where
  loop (NeedMore contentLength chunks) = do
    x <- MaybeT $ TCP.recv socket chunkSize
    y <- throwLeft $ parseChunkLoop (WithMore contentLength chunks x)
    loop y
  loop (Done body remaining) = pure (body, remaining)
  loop (UnParsable s) = do
    x <- MaybeT $ TCP.recv socket chunkSize
    y <- throwLeft $ parseChunkLoop (Initial (s <> x))
    loop y
  loop Failed = error "failed parsing"

  chunkSize = 32768

createReadLoop :: (MonadIO m, MonadThrow m) => Socket -> (BS.ByteString -> m a) -> m b -> m b
createReadLoop socket handleBody onNoChunk = do
  loop ""
 where
  loop r = do
    md <- recieve socket r
    case md of
      Nothing -> onNoChunk
      Just (d, rem) -> do
        void $ handleBody d
        loop rem
