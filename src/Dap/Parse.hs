module Dap.Parse where

import Control.Applicative (many)
import Control.Monad (void)
import Data.Attoparsec.ByteString qualified as P
import Data.Attoparsec.ByteString.Char8 qualified as PC
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

parseHead :: P.Parser BS.ByteString
parseHead = do
  _ <- P.takeWhile (/= 67)
  void $ P.string "Content-Length"
  spaces
  void $ P.string ":"
  spaces
  _contentLength <- fromMaybe (error "content length parse error") . readMaybe @Int . tail . init . show <$> P.takeWhile (not . PC.isSpace_w8)
  P.string "\r\n\r\n"

data ParseArg
  = Initial BS.ByteString
  | WithMore Int [BS.ByteString] BS.ByteString

data ParseResult
  = NeedMore Int [BS.ByteString]
  | Done BS.ByteString BS.ByteString
  | UnParsable BS.ByteString
  | Failed
  deriving (Show)

parseChunkLoop :: ParseArg -> Either String ParseResult
parseChunkLoop (arg :: ParseArg) = do
  case arg of
    Initial s -> flip P.parseOnly s $ do
      _ <- P.takeWhile (/= 67)
      void $ P.string "Content-Length"
      spaces
      void $ P.string ":"
      spaces
      mcontentLength <- readMaybe @Int . tail . init . show <$> P.takeWhile (not . PC.isSpace_w8)
      void $ P.string "\r\n\r\n"
      bodyChunk <- BS.pack <$> many P.anyWord8
      let bodyLength = BS.length bodyChunk
      case mcontentLength of
        Nothing -> pure Failed
        Just contentLength ->
          if bodyLength < contentLength
            then pure $ NeedMore contentLength [bodyChunk]
            else
              let (body, remaining) = BS.splitAt contentLength bodyChunk
               in pure $ Done body remaining
    WithMore contentLength chunks newBody ->
      let chunksBodyLength = sum (map BS.length chunks)
       in if (chunksBodyLength + BS.length newBody) < contentLength
            then pure $ NeedMore contentLength (newBody : chunks)
            else
              let (body, remaining) = BS.splitAt (contentLength - chunksBodyLength) newBody
               in pure $ Done (BS.concat (reverse (body : chunks))) remaining

spaces :: P.Parser ()
spaces = P.skipMany (P.satisfy (== 32))
