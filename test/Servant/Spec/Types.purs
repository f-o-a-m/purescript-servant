module Servant.Spec.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Servant.API (class EncodeQueryParam)
import Servant.API as API


-- API Types for Client and Server

--------------------------------------------------------------------------------
-- | Username
--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- | PhotoID
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- | Photo
--------------------------------------------------------------------------------
newtype Photo =
  Photo { username :: Username
        , title :: String
        , _data :: String
        , photoID :: PhotoID
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

--------------------------------------------------------------------------------
-- | PostPhotoBody
--------------------------------------------------------------------------------
newtype PostPhotoBody =
  PostPhotoBody { username :: Username
                , title :: String
                , _data :: String
                }

derive instance genericPostPhotoBody :: Generic PostPhotoBody _
derive instance newtypePostPhotoBody :: Newtype PostPhotoBody _
derive newtype instance eqPostPhotoBody :: Eq PostPhotoBody
instance showPostPhotoBody :: Show PostPhotoBody where show = genericShow

instance encodePostPhotoBody :: Encode PostPhotoBody where
  encode = genericEncode defaultOptions

instance decodePostPhotoBody :: Decode PostPhotoBody where
  decode = genericDecode defaultOptions

instance encodeJsonPostPhotoBody :: EncodeJson PostPhotoBody where
  encodeJson (PostPhotoBody photo) = encodeJson photo

instance decodeJsonPostPhotoBody :: DecodeJson PostPhotoBody where
  decodeJson x = PostPhotoBody <$> decodeJson x

--------------------------------------------------------------------------------
-- | PostPhotoResponse
--------------------------------------------------------------------------------
newtype PostPhotoResponse =
  PostPhotoResponse { photoID :: PhotoID
                    }

derive instance genericPostPhotoResponse :: Generic PostPhotoResponse _
derive instance newtypePostPhotoResponse :: Newtype PostPhotoResponse _
derive newtype instance eqPostPhotoResponse :: Eq PostPhotoResponse
instance showPostPhotoResponse :: Show PostPhotoResponse where show = genericShow

instance encodePostPhotoResponse :: Encode PostPhotoResponse where
  encode = genericEncode defaultOptions

instance decodePostPhotoResponse :: Decode PostPhotoResponse where
  decode = genericDecode defaultOptions

instance encodeJsonPostPhotoResponse :: EncodeJson PostPhotoResponse where
  encodeJson (PostPhotoResponse photo) = encodeJson photo

instance decodeJsonPostPhotoResponse :: DecodeJson PostPhotoResponse where
  decodeJson x = PostPhotoResponse <$> decodeJson x
