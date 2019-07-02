module Servant.Spec.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (cons, elem, filter, head, take)
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Servant.API as API

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

-- newtypes
newtype Username = Username String

instance eqUsername :: Eq Username where
  eq (Username u1) (Username u2) = u1 == u2

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
        , photoID :: Maybe PhotoID
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

--------------------------------------------------------------------------------
-- | Mock DB
--------------------------------------------------------------------------------

newtype PhotoDB =
  PhotoDB { publicPhotos :: Array Photo
          , privatePhotos :: Map String Photo
          , index :: Int
          }

emptyPhotoDB :: PhotoDB
emptyPhotoDB = PhotoDB {publicPhotos : mempty, privatePhotos: mempty, index: 0}

insertPublicPhoto :: Photo -> AVar.AVar PhotoDB -> Aff Unit
insertPublicPhoto (Photo photo) dbVar = do
  PhotoDB db <- AVar.take dbVar
  let currentIndex = db.index
      photo' = Photo photo { photoID = Just $ PhotoID currentIndex }
  AVar.put (PhotoDB db {publicPhotos = cons photo' db.publicPhotos, index = db.index + 1}) dbVar

insertPrivatePhoto :: Photo -> AVar.AVar PhotoDB -> Aff Unit
insertPrivatePhoto (Photo p@{username: Username u}) dbVar = do
  PhotoDB db <- AVar.take dbVar
  let currentIndex = db.index
      photo' = Photo p { photoID = Just $ PhotoID currentIndex }
  AVar.put (PhotoDB db {privatePhotos = insert u photo' db.privatePhotos, index = db.index + 1}) dbVar

getPhotoById :: PhotoID -> AVar.AVar PhotoDB -> Aff (Maybe Photo)
getPhotoById pid dbVar = do
  PhotoDB {publicPhotos} <- AVar.take dbVar
  pure $ head $
    filter (\(Photo{photoID}) -> photoID == Just pid) publicPhotos

searchPhotos :: Filters -> AVar.AVar PhotoDB -> Aff (Array Photo)
searchPhotos fs dbVar = do
  PhotoDB {publicPhotos} <- AVar.take dbVar
  let fFrom = maybe identity (\i -> filterByIndex (i < _)) fs.fromIndex
      fTo = maybe identity (\i -> filterByIndex (i > _)) fs.toIndex
      fUsername = maybe identity filterByUsername fs.username
      fCount = maybe identity take fs.maxCount
  pure $ (fFrom >>> fTo >>> fUsername >>> fCount) publicPhotos

--------------------------------------------------------------------------------
-- filters
--------------------------------------------------------------------------------

type Filters = Record ( fromIndex :: Maybe Int
                      , toIndex :: Maybe Int
                      , username :: Maybe (Array String)
                      , maxCount :: Maybe Int
                      )

filterByIndex :: (Int -> Boolean) -> Array Photo -> Array Photo
filterByIndex p = filter \(Photo photo) ->
  case photo.photoID of
    Nothing -> false
    Just (PhotoID pId) -> p pId

filterByUsername :: Array String -> Array Photo -> Array Photo
filterByUsername us = filter \(Photo photo) ->
  photo.username `elem` map Username us
