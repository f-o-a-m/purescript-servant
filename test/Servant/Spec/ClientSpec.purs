module Servant.Spec.ClientSpec (spec, mkClientEnv) where


import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as C
import Servant.API as API
import Servant.Client (ClientEnv(..), ErrorDescription)
import Servant.Client.Error (errorDescription)
import Servant.Spec.PhotoClient (ClientM, getHome, getPhotoByID, postPublicPhoto, runClientM)
import Servant.Spec.PhotoServer (startApp)
import Servant.Spec.Types (Photo(..), Username(..))
import Test.Spec (Spec, SpecT(..), beforeAll_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

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

  describe "Path Components" do
    it "Can reach /home" do
      eRes <- runClientM' clientEnv getHome
      case eRes of
        Right resp -> resp `shouldEqual` "home"
        Left err -> fail (show err)

  describe "Capture" do
    it "can post a public photo" do
      let photo = Photo { username: alice
                        , title: "foo"
                        , photoID: Nothing
                        }
      eRes <- runClientM' clientEnv $ do
        C.log $ "posting photo"
        photoID <- postPublicPhoto photo
        C.log $ show photoID
        getPhotoByID $ API.capture (SProxy :: SProxy "photoID") photoID
      case eRes of
        Right (Photo {title}) -> title `shouldEqual` "foo"
        Left err -> fail (show err)
