module Servant.Spec.ClientSpec where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Servant.API (type (:>))
import Servant.API as API
import Servant.Client as Client


spec :: Effect Unit
spec = do
  log "You should add some tests."


--------------------------------------------------------------------------------
-- ClientM
--------------------------------------------------------------------------------


newtype ClientM a = ClientM (ReaderT Client.ClientEnv (ExceptT Client.AjaxError Aff) a)

derive newtype instance functorClientM :: Functor ClientM
derive newtype instance applyClientM :: Apply ClientM
derive newtype instance applicativeClientM :: Applicative ClientM
derive newtype instance bindClientM :: Bind ClientM
derive newtype instance monadClientM :: Monad ClientM
derive newtype instance monadEffClientM :: MonadEffect ClientM
derive newtype instance monadAffClientM :: MonadAff ClientM
derive newtype instance monadAskClientM :: MonadAsk Client.ClientEnv ClientM
derive newtype instance monadThrowClientM :: MonadThrow Client.AjaxError ClientM
derive newtype instance monadErrorClientM :: MonadError Client.AjaxError ClientM

instance runClientClientM :: Client.RunRequest ClientM where
  runRequest = Client.defaultRunRequest

runClientM
  :: Client.ClientEnv
  -> (forall a. ClientM a -> Aff (Either Client.AjaxError a))
runClientM env (ClientM m) = runReaderT m env # runExceptT

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

-- newtypes
newtype Username = Username String

instance encodeJsonUsername :: EncodeJson Username where
  encodeJson (Username u) = encodeJson u

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson x = Username <$> decodeJson x

newtype PhotoID = PhotoID String

instance encodeJsonPhotoID :: EncodeJson PhotoID where
  encodeJson (PhotoID a) = encodeJson a

instance decodeJsonPhotoID :: DecodeJson PhotoID where
  decodeJson x = PhotoID <$> decodeJson x

instance toCapturePhotoID :: API.ToCapture PhotoID where
  toCapture (PhotoID a) = a

-- data
newtype Photo =
  Photo { username :: Username
        , photoID :: Int
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
-- Capture and PathComponent
--------------------------------------------------------------------------------

type GetPhotoByID =
     API.S "photos"
  :> API.CAP "photoID" PhotoID
  :> API.GET Json Photo

getPhotoByID :: API.Capture "photoID" PhotoID -> ClientM Photo
getPhotoByID =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetPhotoByID)

--------------------------------------------------------------------------------
-- Query Params
--------------------------------------------------------------------------------

newtype Date = Date String

instance encodeQueryParamData :: API.EncodeQueryParam Date where
  encodeQueryParam (Date d) = d

type SearchPhotos =
     API.S "photos"
  :> API.QPs ( fromDate :: Maybe Date
             , toDate :: Maybe Date
             , username :: Array String
             , page :: API.Required Int
             )
  :> API.GET Json (Array Photo)

searchPhotos
  :: API.QueryParams ( fromDate :: Maybe Date
                     , toDate :: Maybe Date
                     , username :: Array String
                     , page :: API.Required Int
                     )
  -> ClientM (Array Photo)
searchPhotos =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy SearchPhotos)


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

type PostPublicPhoto =
     API.S "photo"
  :> API.S "public"
  :> API.Body Json Photo
  :> API.POST Json API.NoContent

postPublicPhoto
  :: Photo
  -> ClientM API.NoContent
postPublicPhoto =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy PostPublicPhoto)

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

newtype AuthToken = AuthToken String

instance toHeaderAuthToken :: API.ToHeader AuthToken where
  toHeader (AuthToken at) = at

type PostPrivatePhoto =
     API.S "photo"
  :> API.S "private"
  :> API.Body Json Photo
  :> API.HDRs ("Authorization" :: AuthToken)
  :> API.POST Json API.NoContent

postPrivatePhoto
  :: Photo
  -> API.Headers ("Authorization" :: AuthToken)
  -> ClientM API.NoContent
postPrivatePhoto =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy PostPrivatePhoto)
