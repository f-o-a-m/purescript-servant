module Servant.Spec.PhotoServer
  ( startApp
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array (cons, elem, filter, head, take)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, Error, error, message)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (Foreign, unsafeFromForeign)
import Node.Express.App (App, get, listenHttp, post, setProp, useExternal, useOnError)
import Node.Express.Handler (Handler, HandlerM, nextThrow)
import Node.Express.Request (getBody', getQueryParam, getQueryParams, getRequestHeader, getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (Response, Request)
import Node.HTTP (Server)
import Servant.Spec.Types (Photo(..), PhotoID(..), Username(..))
--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------
foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

type AppState = AVar.AVar PhotoDB

-- NOTE we use this getBody and sendResponse so same decoders/encoders are used as by the client
-- ideally we should not need this
getBody :: forall a. DecodeJson a => HandlerM (Either String a)
getBody =
  -- we get the body as Foreign
  getBody'
    -- because we use jsonBodyParser it must be valid json object
    -- ot express will fail before we get here so we coerce Foreign to Json type 
    <#> (unsafeFromForeign :: Foreign -> Json)
    -- and then we use `decodeJson`
    >>> decodeJson


sendResponse :: forall j. EncodeJson j => Int -> j -> HandlerM Unit
sendResponse code resp = do
  setStatus code
  sendJson $ encodeJson resp

appSetup :: AppState -> App
appSetup state = do
  useExternal jsonBodyParser
  setProp "json spaces" 4.0
  get "/home" getHome
  get "/photos/:photoID" $ getPhotoByIDHandler state
  get "/photos" $ searchPhotosHandler state
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
getHome = sendResponse 200 "home"

getPhotoByIDHandler :: AppState -> Handler
getPhotoByIDHandler state = do
  midParam <- getRouteParam "photoID"
  case midParam >>= fromString of
    Just idParam -> do
      mPhoto <- liftAff $ getPhotoById (PhotoID idParam) state
      case mPhoto of
        Nothing -> sendResponse 404 "oops"
        Just photo -> sendResponse 200 photo
    Nothing -> nextThrow $ error "photoID is required?"

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
    Nothing -> sendResponse 421 "oops"
    Just _ -> do
      ps <- liftAff $ searchPhotos {fromIndex, toIndex, maxCount, username} state
      sendResponse 200 ps

postPublicPhotoHandler :: AppState -> Handler
postPublicPhotoHandler state = do
  ePhoto <- getBody
  case ePhoto of
    Left err -> nextThrow $ error $ "Couldn't parse Photo: " <> err
    Right photo -> do
      _id <- liftAff $ insertPublicPhoto photo state
      sendResponse 200 $ PhotoID _id

postPrivatePhotoHandler :: AppState -> Handler
postPrivatePhotoHandler state = do
  log "postPrivatePhotoHandler"
  mAuthHeader <- getRequestHeader "Authorization"
  log $ show mAuthHeader
  case mAuthHeader of
    Nothing -> sendResponse 403 "oops"
    Just username -> do
      ePhoto <- getBody
      log $ show ePhoto
      case ePhoto of
        Left err -> nextThrow $ error $ "Couldn't parse Photo: " <> err
        Right photo -> do
          _id <- liftAff $ insertPrivatePhoto photo state
          sendResponse 200 $ PhotoID _id

errorHandler :: Error -> Handler
errorHandler err = do
  sendResponse 400 {error: message err}

--------------------------------------------------------------------------------
-- | Mock DB
--------------------------------------------------------------------------------

overAVar :: forall r a. (a -> { res :: r, val :: a}) -> AVar a -> Aff r
overAVar f var = do
  v <- AVar.take var
  let {res, val} = f v
  AVar.put val var
  pure res

newtype PhotoDB =
  PhotoDB { publicPhotos :: Array Photo
          , privatePhotos :: Map String Photo
          , index :: Int
          }

emptyPhotoDB :: PhotoDB
emptyPhotoDB = PhotoDB {publicPhotos : mempty, privatePhotos: mempty, index: 0}

insertPublicPhoto :: Photo -> AVar.AVar PhotoDB -> Aff Int
insertPublicPhoto (Photo photo) = overAVar \(PhotoDB db) ->
  let
    currentIndex = db.index
    photo' = Photo photo { photoID = Just $ PhotoID currentIndex }
  in 
    { val: PhotoDB db {publicPhotos = cons photo' db.publicPhotos, index = db.index + 1}
    , res: currentIndex
    }

insertPrivatePhoto :: Photo -> AVar.AVar PhotoDB -> Aff Int
insertPrivatePhoto (Photo p@{username: Username u}) = overAVar \(PhotoDB db) ->
  let
    currentIndex = db.index
    photo' = Photo p { photoID = Just $ PhotoID currentIndex }
  in
    { val: PhotoDB db {privatePhotos = insert u photo' db.privatePhotos, index = db.index + 1}
    , res: currentIndex
    }

getPhotoById :: PhotoID -> AVar.AVar PhotoDB -> Aff (Maybe Photo)
getPhotoById pid = AVar.read >=> \(PhotoDB {publicPhotos}) ->
  pure $ head $ filter (\(Photo{photoID}) -> photoID == Just pid) publicPhotos

searchPhotos :: Filters -> AVar.AVar PhotoDB -> Aff (Array Photo)
searchPhotos fs = AVar.read >=> \(PhotoDB {publicPhotos}) ->
  let
    fFrom = maybe identity (\i -> filterByIndex (i < _)) fs.fromIndex
    fTo = maybe identity (\i -> filterByIndex (i > _)) fs.toIndex
    fUsername = maybe identity filterByUsername fs.username
    fCount = maybe identity take fs.maxCount
  in pure $ (fFrom >>> fTo >>> fUsername >>> fCount) publicPhotos

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
  un Username photo.username `elem` us
