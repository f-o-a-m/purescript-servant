module Servant.Spec.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Servant.API as API

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

-- newtypes
newtype Username = Username String

instance eqUsername :: Eq Username where
  eq (Username u1) (Username u2) = u1 == u2

derive instance genericUsername :: Generic Username _

instance encodeUsername :: Encode Username where
  encode = genericEncode defaultOptions

instance decodeUsername :: Decode Username where
  decode = genericDecode defaultOptions

instance encodeJsonUsername :: EncodeJson Username where
  encodeJson (Username u) = encodeJson u

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson x = Username <$> decodeJson x

newtype PhotoID = PhotoID Int

instance eqPhotoID :: Eq PhotoID where
  eq (PhotoID a) (PhotoID b) = a == b

derive instance genericPhotoID :: Generic PhotoID _

instance encodePhotoID :: Encode PhotoID where
  encode = genericEncode defaultOptions

instance decodePhotoID :: Decode PhotoID where
  decode = genericDecode defaultOptions

instance encodeJsonPhotoID :: EncodeJson PhotoID where
  encodeJson (PhotoID a) = encodeJson a

instance decodeJsonPhotoID :: DecodeJson PhotoID where
  decodeJson x = PhotoID <$> decodeJson x

instance toCapturePhotoID :: API.ToCapture PhotoID where
  toCapture (PhotoID a) = show a

-- data
newtype Photo =
  Photo { username :: Username
        , photoID :: Maybe PhotoID
        }

derive instance genericPhoto :: Generic Photo _

instance encodePhoto :: Encode Photo where
  encode = genericEncode defaultOptions

instance decodePhoto :: Decode Photo where
  decode = genericDecode defaultOptions

instance encodeJsonPhoto :: EncodeJson Photo where
  encodeJson (Photo photo) = encodeJson photo

instance decodeJsonPhoto :: DecodeJson Photo where
  decodeJson x = Photo <$> decodeJson x

