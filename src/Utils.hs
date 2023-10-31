module Utils where

import Control.Exception.Safe
import Data.Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Foldable qualified as F
import Data.Foldable.WithIndex qualified as F
import Prettyprinter
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

rdrop :: String -> String -> String
rdrop str = reverse . drop (length str) . reverse

throwLeft :: MonadThrow m => Either String b -> m b
throwLeft (Left s) = throwString s
throwLeft (Right x) = pure x

throwNothing :: MonadThrow m => Maybe a -> m a
throwNothing Nothing = throwString "Value was Nothing."
throwNothing (Just x) = pure x

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

str2lbs :: String -> BSL.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack
