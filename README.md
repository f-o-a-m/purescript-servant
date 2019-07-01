# purescript-servant
A servant like DSL for templating requests

## Intro
This library defines a type level DSL à la servant to describe the types of http requests and to implement clients.

## Examples
The following example compiles, we will update this README once we have more tests and docs.

```purescript

module Example where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Argonaut (Json)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Servant.API (type (:>))
import Servant.API as API
import Servant.Client (AjaxError, ClientEnv, makeClientRoute)

type ClientM = ReaderT ClientEnv (ExceptT AjaxError Aff)

type GetPhotos =
     API.S "photos"
  :> API.QPs ( users :: Array Int
             , fromDate :: Maybe String
             , toDate :: API.Required String
             )
  :> API.GET Json String

getPhotos
  :: API.QueryParams ( users :: Array Int
                     , fromDate :: Maybe String
                     , toDate :: API.Required String
                     )
  -> ClientM String
getPhotos = makeClientRoute (API.RouteProxy :: API.RouteProxy GetPhotos)

type PostComment =
     API.S "photos"
  :> API.CAP "photoID" Int
  :> API.Body Json String
  :> API.HDRs ("AuthToken" :: String)
  :> API.POST Json API.NoContent

postComment
  :: API.Capture "userID" Int
  -> String
  -> API.Headers ("AuthToken" :: String)
  -> ClientM API.NoContent
postComment = makeClientRoute (API.RouteProxy :: API.RouteProxy PostComment)



```
