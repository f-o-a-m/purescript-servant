module Servant.Spec.ClientSpec (spec, mkClientEnv) where


import Prelude

import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Class.Console as C
import Servant.API as API
import Servant.Client (ClientEnv(..), ErrorDescription(..))
import Servant.Client.Error (errorDescription)
import Servant.Spec.PhotoClient (AuthToken(..), ClientM, Index(..), getHome, getPhotoByID, postPrivatePhoto, postPublicPhoto, runClientM, searchPhotos)
import Servant.Spec.Types (Photo(..), PhotoID(..), PostPhotoBody(..), PostPhotoResponse(..), Username(..))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

mkClientEnv :: Int -> ClientEnv
mkClientEnv port =
  ClientEnv { protocol: "http"
            , baseURL: "//localhost:" <> show port <> "/"
            }

runClientM'
  :: ClientEnv
  -> (forall a. ClientM a -> Aff (Either ErrorDescription a))
runClientM' env action = do
  eRes <- runClientM env action
  pure $ lmap (view errorDescription) eRes

alice :: Username
alice = Username "Alice"

bob :: Username
bob = Username "Bob"

spec :: ClientEnv -> SpecT Aff Unit Aff Unit
spec clientEnv = do
  let
    assertClientM :: forall a. ClientM a -> Aff a
    assertClientM action = runClientM' clientEnv action >>= case _ of
      Left err -> throwError (error $ show err)
      Right res -> pure res

  describe "Path Components" do
    it "Can reach /home" do
      resp <- assertClientM getHome
      resp `shouldEqual` "home"

  describe "Capture" do
    it "can post a public photo" do
      let photoPost = PostPhotoBody { username: alice, title: "foo", _data: "abc"}
      PostPhotoResponse {photoID} <- assertClientM $ postPublicPhoto photoPost
      Photo photo <- assertClientM $ getPhotoByID (API.capture (SProxy :: SProxy "photoID") photoID)
      photo.photoID `shouldEqual` photoID
    it "can post a private photo" do
      let photoPost = PostPhotoBody { username: bob, title: "blaa", _data: "def"}
      PostPhotoResponse {photoID} <- assertClientM $ postPrivatePhoto photoPost $ API.Headers {"Authorization": AuthToken "Bob"}
      res <- runClientM' clientEnv $ getPhotoByID (API.capture (SProxy :: SProxy "photoID") photoID)
      res `shouldShowEqual` Left (UnexpectedHTTPStatus (StatusCode 404))
    it "can search public photos without returning private photos" do
      photos <- assertClientM $ searchPhotos $ API.QueryParams
        { fromIndex: Nothing
        , toIndex: Nothing
        , username: [bob]
        , maxCount: API.Required 10
        }
      length photos `shouldEqual` 0
    it "can search public photos" do
      photos <- assertClientM $ searchPhotos $ API.QueryParams
        { fromIndex: Just $ Index 0
        , toIndex: Just $ Index 10
        , username: [alice, bob]
        , maxCount: API.Required 10
        }
      length photos `shouldEqual` 1
    it "can search public photos using more params" do
      let photoPost = PostPhotoBody { username: alice, title: "foo2", _data: "ghi"}
      PostPhotoResponse {photoID: PhotoID pid} <- assertClientM $ postPublicPhoto photoPost
      C.log "Getting all of alice's photos"
      photos <- assertClientM $ searchPhotos $ API.QueryParams
        { fromIndex: Just $ Index 0
          , toIndex: Just $ Index pid
          , username: [alice]
          , maxCount: API.Required 2
          }
      length photos `shouldEqual` 2
      C.log "Getting all of alice's photos except the last one"
      photosMinusLast <- assertClientM $ searchPhotos $ API.QueryParams
        { fromIndex: Just $ Index 0
        , toIndex: Just $ Index (pid - 1)
        , username: [alice]
        , maxCount: API.Required 2
        }
      length photosMinusLast `shouldEqual` 1
      C.log "Getting a portion of alice's photos"
      photosSlice <- assertClientM $ searchPhotos $ API.QueryParams
        { fromIndex: Just $ Index 1
        , toIndex: Just $ Index pid
        , username: [alice]
        , maxCount: API.Required 1
        }
      length photosSlice `shouldEqual` 1


shouldShowEqual :: forall m t. MonadThrow Error m => Show t => t -> t -> m Unit
shouldShowEqual a b = show a `shouldEqual` show b
