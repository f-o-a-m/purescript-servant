module Servant.Spec.PhotoServer
  ( startApp
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (cons, elem, filter, head, take)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..), maybe)
import Debug.Trace as Trace
import Effect.Aff (Aff, Error, error, message)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Express.App (App, get, listenHttp, post, setProp, useOnError)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getBody, getBody', getQueryParam, getQueryParams, getRequestHeader, getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import Node.HTTP (Server)
import Servant.API as API
import Servant.Spec.Types (Photo(..), PhotoID(..), Username(..))

--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

type AppState = AVar.AVar PhotoDB

appSetup :: AppState -> App
appSetup state = do
  setProp "json spaces" 4.0
  get "/home" getHome
  get "/photos/:photoID" $ getPhotoByIDHandler state
  get "/photos/search" $ searchPhotosHandler state
  post "/photos/public" $ postPublicPhotoHandler state
  post "/photos/private" $ postPrivatePhotoHandler state
  useOnError errorHandler

startApp :: Int -> Aff Server
startApp port = do
  state <- AVar.new emptyPhotoDB
  liftEffect $ listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

getHome :: Handler
getHome = sendJson "home"

getPhotoByIDHandler :: AppState -> Handler
getPhotoByIDHandler state = do
  midParam <- getRouteParam "photoID"
  case midParam >>= fromString of
    Just idParam -> do
      mPhoto <- liftAff $ getPhotoById (PhotoID idParam) state
      case mPhoto of
        Nothing -> setStatus 404
        Just photo -> sendJson photo
    Nothing -> nextThrow $ error "photoID is required"

searchPhotosHandler :: AppState -> Handler
searchPhotosHandler state = do
  fromIndex <- do
    mfrom <- getQueryParam "fromIndex"
    pure $ mfrom >>= fromString
  toIndex <- do
    mto <- getQueryParam "toIndex"
    pure $ mto >>= fromString
  maxCount <- do
    mmax <- getQueryParam "maxCount"
    pure $ mmax >>= fromString
  username <- getQueryParams "username"
  case maxCount of
    Nothing -> setStatus 421
    Just _ -> do
      ps <- liftAff $ searchPhotos {fromIndex, toIndex, maxCount, username} state
      sendJson ps

postPublicPhotoHandler :: AppState -> Handler
postPublicPhotoHandler state = do
  getBody' >>= Trace.traceM
  ephoto <- runExcept <$> getBody
  case ephoto of
    Left err -> nextThrow $ error ("Couldn't parse Photo: " <> show err)
    Right photo -> do
      _id <- liftAff $ insertPublicPhoto photo state
      sendJson $ PhotoID _id

postPrivatePhotoHandler :: AppState -> Handler
postPrivatePhotoHandler state = do
  mAuthHeader <- getRequestHeader "Authorization"
  case mAuthHeader of
    Nothing -> setStatus 403
    Just username -> do
      ephoto <- runExcept <$> getBody
      case ephoto of
        Left err -> nextThrow $ error "Couldn't parse Photo"
        Right photo -> do
          _id <- liftAff $ insertPrivatePhoto photo state
          sendJson $ PhotoID _id

errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

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

insertPublicPhoto :: Photo -> AVar.AVar PhotoDB -> Aff Int
insertPublicPhoto (Photo photo) dbVar = do
  PhotoDB db <- AVar.take dbVar
  let currentIndex = db.index
      photo' = Photo photo { photoID = Just $ PhotoID currentIndex }
  AVar.put (PhotoDB db {publicPhotos = cons photo' db.publicPhotos, index = db.index + 1}) dbVar
  pure currentIndex

insertPrivatePhoto :: Photo -> AVar.AVar PhotoDB -> Aff Int
insertPrivatePhoto (Photo p@{username: Username u}) dbVar = do
  PhotoDB db <- AVar.take dbVar
  let currentIndex = db.index
      photo' = Photo p { photoID = Just $ PhotoID currentIndex }
  AVar.put (PhotoDB db {privatePhotos = insert u photo' db.privatePhotos, index = db.index + 1}) dbVar
  pure currentIndex

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
-- search filters
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
