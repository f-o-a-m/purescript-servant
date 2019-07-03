module Servant.Spec.ClientSpec (spec, mkClientEnv) where


import Prelude

import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Class.Console as C
import Servant.API as API
import Servant.Client (ClientEnv(..), ErrorDescription(..))
import Servant.Client.Error (errorDescription)
import Servant.Spec.PhotoClient (AuthToken(..), ClientM, getHome, getPhotoByID, postPrivatePhoto, postPublicPhoto, runClientM, searchPhotos)
import Servant.Spec.Types (Photo(..), Username(..))
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
      let photo = Photo { username: alice, title: "foo", photoID: Nothing}
      C.log $ "posting photo"
      photoID <- assertClientM $ postPublicPhoto photo
      C.log $ show photoID
      photo' <- assertClientM $ getPhotoByID (API.capture (SProxy :: SProxy "photoID") photoID)
      photo' `shouldEqual` over Photo _{photoID = Just photoID} photo
    it "can post a private photo" do
      let photo = Photo { username: bob, title: "blaa", photoID: Nothing}
      C.log $ "posting photo"
      photoID <- assertClientM $ postPrivatePhoto photo $ API.Headers {"Authorization": AuthToken "tok"}
      C.log $ show photoID
      res <- runClientM' clientEnv $ getPhotoByID (API.capture (SProxy :: SProxy "photoID") photoID)
      res `shouldShowEqual` Left (UnexpectedHTTPStatus (StatusCode 404))
    it "can search photos" do
      let photo = Photo { username: bob, title: "blaa", photoID: Nothing}
      C.log $ "searching photo"
      photos <- assertClientM $ searchPhotos $ API.QueryParams 
        { fromIndex: Nothing
        , toIndex: Nothing
        , username: [alice, bob]
        , maxCount: API.Required 10
        }
      length photos `shouldEqual` 1


shouldShowEqual :: forall m t. MonadThrow Error m => Show t => t -> t -> m Unit
shouldShowEqual a b = show a `shouldEqual` show b