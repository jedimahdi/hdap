module Utils where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL.Char8
import Data.Foldable qualified as F
import Data.Foldable.WithIndex qualified as F
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.Stack (HasCallStack)
import Prettyprinter
import UnliftIO

rdrop :: String -> String -> String
rdrop str = reverse . drop (length str) . reverse

addContentLength :: BSL.ByteString -> BSL.ByteString
addContentLength s =
  "Content-Length: "
    <> BSL.Char8.pack (show (BSL.length s))
    <> "\r\n\r\n"
    <> s

throwLeft :: MonadIO m => Either String b -> m b
throwLeft (Left s) = throwString s
throwLeft (Right x) = pure x

throwNothing :: (HasCallStack) => Maybe a -> IO a
throwNothing Nothing = throwString "Value was Nothing."
throwNothing (Just x) = pure x

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

printJSON :: MonadIO m => Value -> m ()
printJSON = liftIO . print . prettyJSON

prettyJSON :: Value -> Doc ann
prettyJSON = \case
  Array vec ->
    let docs = fmap prettyJSON (F.toList vec)
        separator = ","
     in group $ nest 2 ("[" <> line <> vsep (punctuate separator docs)) <> line <> "]"
  Object km ->
    let docs = fmap (\(k, v) -> pretty (show k) <> ":" <+> prettyJSON v) (F.itoList km)
        separator = ","
     in group $ nest 2 ("{" <> line <> vsep (punctuate separator docs)) <> line <> "}"
  -- for atomic objects, piggyback off aeson's encoding
  v -> pretty $ Aeson.encodeToLazyText v

newtype ViaJSON a = ViaJSON a

instance ToJSON a => Pretty (ViaJSON a) where
  pretty (ViaJSON a) = prettyJSON $ toJSON a

dapOptions :: Options
dapOptions = defaultOptions {omitNothingFields = True}

str2lbs :: String -> BSL.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack
