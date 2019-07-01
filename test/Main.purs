module Test.Main where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Argonaut (Json)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Servant.API (type (:>))
import Servant.API as API
import Servant.Client (AjaxError, ClientEnv, makeClientRoute)

main :: Effect Unit
main = do
  log "You should add some tests."


-- just want to check that these compile
type ClientM = ReaderT ClientEnv (ExceptT AjaxError Aff)

type TestGetR =
     API.S "photos"
  :> API.QPs ( users :: Array Int
             , from :: Maybe String
             , to :: API.Required String
             )
  :> API.GET Json String

testGet
  :: API.QueryParams ( users :: Array Int
                     , from :: Maybe String
                     , to :: API.Required String
                     )
  -> ClientM String
testGet = makeClientRoute (API.RouteProxy :: API.RouteProxy TestGetR)

type TestPostR =
     API.S "photos"
  :> API.CAP "userID" Int
  :> API.Body Json String
  :> API.HDRs ("AuthToken" :: String)
  :> API.POST Json API.NoContent

testPost
  :: API.Capture "userID" Int
  -> String
  -> API.Headers ("AuthToken" :: String)
  -> ClientM API.NoContent
testPost = makeClientRoute (API.RouteProxy :: API.RouteProxy TestPostR)
