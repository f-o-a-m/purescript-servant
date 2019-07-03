module Servant.Spec.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Servant.API (class EncodeQueryParam)
import Servant.API as API

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

-- newtypes
newtype Username = Username String
derive instance newtypeUsername :: Newtype Username _
derive instance genericUsername :: Generic Username _
instance showUsername :: Show Username where show = genericShow
derive newtype instance eqUsername :: Eq Username
derive newtype instance encodeQueryParamUsername :: EncodeQueryParam Username

instance encodeUsername :: Encode Username where
  encode = genericEncode defaultOptions

instance decodeUsername :: Decode Username where
  decode = genericDecode defaultOptions

instance encodeJsonUsername :: EncodeJson Username where
  encodeJson (Username u) = encodeJson u

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson x = Username <$> decodeJson x

newtype PhotoID = PhotoID Int
derive instance newtypePhotoID :: Newtype PhotoID _
derive instance genericPhotoID :: Generic PhotoID _
derive newtype instance eqPhotoID :: Eq PhotoID
instance showPhotoID :: Show PhotoID where show = genericShow

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
        , title :: String
        , photoID :: Maybe PhotoID
        }

derive instance genericPhoto :: Generic Photo _
derive instance newtypePhoto :: Newtype Photo _
derive newtype instance eqPhoto :: Eq Photo
instance showPhoto :: Show Photo where show = genericShow

instance encodePhoto :: Encode Photo where
  encode = genericEncode defaultOptions

instance decodePhoto :: Decode Photo where
  decode = genericDecode defaultOptions

instance encodeJsonPhoto :: EncodeJson Photo where
  encodeJson (Photo photo) = encodeJson photo

instance decodeJsonPhoto :: DecodeJson Photo where
  decodeJson x = Photo <$> decodeJson x

