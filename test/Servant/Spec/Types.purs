module Servant.Spec.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (cons, filter, head)
import Data.Map (Map, insert)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Servant.API as API

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

-- newtypes
newtype Username = Username String

instance encodeJsonUsername :: EncodeJson Username where
  encodeJson (Username u) = encodeJson u

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson x = Username <$> decodeJson x

newtype PhotoID = PhotoID Int

instance eqPhotoID :: Eq PhotoID where
  eq (PhotoID a) (PhotoID b) = a == b

instance encodeJsonPhotoID :: EncodeJson PhotoID where
  encodeJson (PhotoID a) = encodeJson a

instance decodeJsonPhotoID :: DecodeJson PhotoID where
  decodeJson x = PhotoID <$> decodeJson x

instance toCapturePhotoID :: API.ToCapture PhotoID where
  toCapture (PhotoID a) = show a

-- data
newtype Photo =
  Photo { username :: Username
        , photoID :: PhotoID
        }

instance encodeJsonPhoto :: EncodeJson Photo where
  encodeJson (Photo d) =
    "username" := d.username ~>
    "photoID" := d.photoID ~>
    jsonEmptyObject

instance decodeJsonPhoto :: DecodeJson Photo where
  decodeJson obj = do
    o <- decodeJson obj
    username <- o .: "username"
    photoID <- o .: "photoID"
    pure $ Photo {username, photoID}


newtype PhotoDB =
  PhotoDB { publicPhotos :: Array Photo
          , privatePhotos :: Map String Photo
          }

emptyPhotoDB :: PhotoDB
emptyPhotoDB = PhotoDB {publicPhotos : mempty, privatePhotos: mempty}

insertPublicPhoto :: Photo -> AVar.AVar PhotoDB -> Aff Unit
insertPublicPhoto photo dbVar = do
  PhotoDB db <- AVar.take dbVar
  AVar.put (PhotoDB db {publicPhotos = cons photo db.publicPhotos}) dbVar

insertPrivatePhoto :: Photo -> AVar.AVar PhotoDB -> Aff Unit
insertPrivatePhoto photo@(Photo {username: Username u}) dbVar = do
  PhotoDB db <- AVar.take dbVar
  AVar.put (PhotoDB db {privatePhotos = insert u photo db.privatePhotos}) dbVar

getPhotoById :: PhotoID -> AVar.AVar PhotoDB -> Aff (Maybe Photo)
getPhotoById pid dbVar = do
  PhotoDB {publicPhotos} <- AVar.take dbVar
  pure $ head $
    filter (\(Photo{photoID}) -> photoID == pid) publicPhotos
