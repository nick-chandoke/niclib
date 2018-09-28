-- | Superset of Aeson; introduces 3 functions that just make JSON parsing easy: (pickJSONField "{k:'v'}" "k" :: Text) --> "v"
-- exports Aeson, too; just import NicLib.Aeson instead of Data.Aeson.
module NicLib.Aeson
( module Data.Aeson.Encode.Pretty
, module Data.Aeson
, module Data.Aeson.TH
, eitherDecode
, pickJSONField
, pickJSONFieldObj
) where

import Data.Aeson hiding (eitherDecode)
import Data.Aeson.Encode.Pretty -- package aeson-pretty. Used only for re-export
import Data.Aeson.TH
import qualified Data.Aeson (eitherDecode)
import qualified Data.Bifunctor as BiF
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T'
import Data.ListLike

-- | override Aeson's version to return (Either s a) rather than (Either String a)
eitherDecode :: (FromJSON a, StringLike str) => BS.ByteString -> Either str a
eitherDecode = BiF.first fromString . Data.Aeson.eitherDecode

-- | very convenient method to get an attribute from a JSON blob. Keeps Aeson operators in order.
-- due to type-matching context constraints (namely matching T'.Text -> Parser a, using a's FromJSON context binding in pickJSONField's method signature), we have to use HashMap rather than (.:)
-- so really pickJSONField should be used if picking only one field from a JSON ByteString
-- remember to specify type for FromJSON a, e.g. pickJSONField blob attr :: T'.Text
pickJSONField :: forall a str. (StringLike str, FromJSON a) => BS.ByteString -> T'.Text -> Either str a
pickJSONField blob attr = NicLib.Aeson.eitherDecode blob >>= flip pickJSONFieldObj attr

-- | version of pickJSONField that takes an Object rather than a lazy ByteString; because it uses an already parsed object, it guarantees that no object will be parsed more than once (as may be the case if one passed the same ByteString to many calls to pickJSONField; I'd hope that GHC -O2 would be smart enough to make only one call, but that's an implementation detail I can't guarantee.)
-- remember to specify type for FromJSON a, e.g. pickJSONFieldObj blob attr :: T'.Text
pickJSONFieldObj :: forall a str. (StringLike str, FromJSON a) => Object -> T'.Text -> Either str a
pickJSONFieldObj obj attr =
    flip (maybe (Left $ fromString "JSON parse error")) (HM.lookup attr obj >>= \value -> return (fromJSON value :: Result a)) $ \case
        Error str -> Left $ fromString str
        Success s -> Right s
