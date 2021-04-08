module Servant.Spec.PhotoServer
  ( startApp
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind (bindFlipped)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Array (cons, elem, filter, head, take)
import Data.Either (Either(..), hush)
import Data.EitherR (fmapL)
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error, error, message)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (Foreign, readArray, readString, unsafeFromForeign)
import Node.Express.App (App, get, listenHttp, post, setProp, useExternal, useOnError)
import Node.Express.Handler (Handler, HandlerM, nextThrow)
import Node.Express.Request (getBody', getQueryParam, getRequestHeader, getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (Response, Request)
import Node.HTTP (Server)
import Servant.Spec.Types (Photo(..), PhotoID(..), PostPhotoBody(..), PostPhotoResponse(..), Username(..))

--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------
foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

type AppState =
  { photoDB :: AVar.AVar PhotoDB
  }

-- NOTE we use this getBody and sendResponse so same decoders/encoders are used as by the client
-- ideally we should not need this
getBody
  :: forall a.
     DecodeJson a
  => HandlerM (Either String a)
getBody =
  -- we get the body as Foreign
  getBody'
    -- because we use jsonBodyParser it must be valid json object
    -- ot express will fail before we get here so we coerce Foreign to Json type 
    <#> (unsafeFromForeign :: Foreign -> Json)
    -- and then we use `decodeJson`
    >>> decodeJson
    >>> fmapL printJsonDecodeError


sendResponse
  :: forall j.
     EncodeJson j
  => Int
  -> j
  -> HandlerM Unit
sendResponse code resp = do
  setStatus code
  sendJson $ encodeJson resp

appSetup
  :: AppState
  -> App
appSetup state = do
  useExternal jsonBodyParser
  setProp "json spaces" 4.0
  get "/home" getHome
  get "/photos/:photoID" $ getPhotoByIDHandler state
  get "/photos" $ searchPhotosHandler state
  post "/photos/public" $ postPublicPhotoHandler state
  post "/photos/private" $ postPrivatePhotoHandler state
  useOnError errorHandler

startApp
  :: Int
  -> Aff Server
startApp port = do
  photoDB <- AVar.new emptyPhotoDB
  let state = {photoDB}
  liftEffect $ listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

getHome :: Handler
getHome = sendResponse 200 "home"

getPhotoByIDHandler
  :: AppState
  -> Handler
getPhotoByIDHandler state = do
  midParam <- getRouteParam "photoID"
  case midParam >>= fromString of
    Just idParam -> do
      mPhoto <- runDB state $ getPhotoById (PhotoID idParam)
      case mPhoto of
        Nothing -> sendResponse 404 "oops"
        Just photo -> sendResponse 200 photo
    Nothing -> nextThrow $ error "photoID is required?"

searchPhotosHandler
  :: AppState
  -> Handler
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
  username <- getQueryParam "username" <#> bindFlipped \f ->
    hush $ runExcept $ map pure (readString f) <|> (readArray f >>= traverse readString)
  case maxCount of
    Nothing -> sendResponse 421 "oops"
    Just _ -> do
      ps <- runDB state $ searchPhotos {fromIndex, toIndex, maxCount, username}
      sendResponse 200 ps

postPublicPhotoHandler
  :: AppState
  -> Handler
postPublicPhotoHandler state = do
  ePhoto <- getBody
  case ePhoto of
    Left err -> nextThrow $ error $ "Couldn't parse Photo: " <> err
    Right (PostPhotoBody photo) -> do
      photoID <- runDB state $ insertPublicPhoto photo
      sendResponse 200 $ PostPhotoResponse {photoID}

postPrivatePhotoHandler
  :: AppState
  -> Handler
postPrivatePhotoHandler state = do
  mAuthHeader <- getRequestHeader "Authorization"
  case mAuthHeader of
    Nothing -> sendResponse 403 "oops"
    Just username -> do
      ePhoto <- getBody
      case ePhoto of
        Left err -> nextThrow $ error "Couldn't parse Photo"
        Right (PostPhotoBody photo@{username: Username username'}) -> do
          if username == username'
            then do
              photoID <- runDB state $ insertPrivatePhoto photo
              sendResponse 200 $ PostPhotoResponse {photoID}
            else setStatus 403

errorHandler
  :: Error
  -> Handler
errorHandler err = do
  sendResponse 400 {error: message err}

--------------------------------------------------------------------------------
-- | DB
--------------------------------------------------------------------------------

-- Photo type for writing to DB
type PhotoData =
  { username :: Username
  , _data :: String
  , title :: String
  }

newtype PhotoDB =
  PhotoDB { publicPhotos :: Array Photo
          , privatePhotos :: Map String Photo
          , index :: Int
          }

emptyPhotoDB :: PhotoDB
emptyPhotoDB =
  PhotoDB { publicPhotos : mempty
          , privatePhotos: mempty
          , index: 0
          }

-- DB utils
runDB
  :: forall m.
     MonadAff m
  => AppState
  -> ReaderT AppState Aff ~> m
runDB env = liftAff <<< flip runReaderT env

withDB
  :: forall r m.
     MonadAsk AppState m
  => MonadAff m
  => (PhotoDB -> { res :: r, db :: PhotoDB })
  -> m r
withDB f = do
  {photoDB} <- ask
  v <- liftAff $ AVar.take photoDB
  let {res, db} = f v
  liftAff $ AVar.put db photoDB
  pure res

withDB_
  :: forall m.
     MonadAsk AppState m
  => MonadAff m
  => (PhotoDB -> PhotoDB)
  -> m Unit
withDB_ f = withDB \db ->
  { res: unit
  , db: f db
  }

queryDB
  :: forall r m.
     MonadAsk AppState m
  => MonadAff m
  => (PhotoDB -> r)
  -> m r
queryDB f = do
  {photoDB} <- ask
  liftAff $ do
    v <- AVar.take photoDB
    AVar.put v photoDB
    pure $ f v

createPhoto
  :: forall m.
     MonadAsk AppState m
  => MonadAff m
  => PhotoData
  -> m Photo
createPhoto {username, _data, title} = do
  i <- queryDB \(PhotoDB {index: currentIndex}) -> currentIndex
  pure $ Photo { username
               , _data
               , title
               , photoID: PhotoID i
               }

insertPublicPhoto
  :: forall m.
     MonadAsk AppState m
  => MonadAff m
  => PhotoData
  -> m PhotoID
insertPublicPhoto photoData = do
  photo <- createPhoto photoData
  let Photo{photoID} = photo
  withDB_ \(PhotoDB pdb) ->
    PhotoDB pdb { publicPhotos = cons photo pdb.publicPhotos
                , index = pdb.index + 1
                }
  pure photoID

insertPrivatePhoto
  :: forall m.
     MonadAsk AppState m
  => MonadAff m
  => PhotoData
  -> m PhotoID
insertPrivatePhoto photoData@{username: Username u} = do
  photo <- createPhoto photoData
  let Photo{photoID} = photo
  withDB_ \(PhotoDB pdb) ->
    PhotoDB pdb { privatePhotos = insert u photo pdb.privatePhotos
                , index = pdb.index + 1
                }
  pure photoID

getPhotoById
  :: forall m.
     MonadAsk AppState m
  => MonadAff m
  => PhotoID
  -> m (Maybe Photo)
getPhotoById pid = queryDB \(PhotoDB {publicPhotos}) ->
  head $ filter (\(Photo{photoID}) -> photoID == pid) publicPhotos

searchPhotos
  :: forall m.
     MonadAsk AppState m
  => MonadAff m
  => Filters
  -> m (Array Photo)
searchPhotos fs = queryDB \(PhotoDB {publicPhotos}) ->
  let
    fFrom = maybe identity (\i -> filterByIndex (i <= _)) fs.fromIndex
    fTo = maybe identity (\i -> filterByIndex (i >= _)) fs.toIndex
    fUsername = maybe identity filterByUsername fs.username
    fCount = maybe identity take fs.maxCount
  in (fFrom >>> fTo >>> fUsername >>> fCount) publicPhotos

--------------------------------------------------------------------------------
-- search filters
--------------------------------------------------------------------------------

type Filters =
  Record ( fromIndex :: Maybe Int
         , toIndex :: Maybe Int
         , username :: Maybe (Array String)
         , maxCount :: Maybe Int
         )

filterByIndex
  :: (Int -> Boolean)
  -> Array Photo
  -> Array Photo
filterByIndex p = filter \(Photo photo) ->
  case photo.photoID of
    PhotoID pId -> p pId

filterByUsername
  :: Array String
  -> Array Photo
  -> Array Photo
filterByUsername us = filter \(Photo photo) ->
  un Username photo.username `elem` us
