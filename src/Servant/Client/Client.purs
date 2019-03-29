module Servant.Client.Client
  ( module Servant.Client.Client
  , module Reexport
  )where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Servant.Client.Request (AjaxError, ClientEnv, affjax, defaultRequest, getResult)
import Servant.Api.Types (QueryParams(..), Captures(..), Required(..), Headers(..)) as Reexport
import Servant.Api.Types (class RouteBuilder, Captures, QueryParams, RouteProxy, Headers, SuspendedRoute(..), buildRoute)
import Network.HTTP.Affjax (AffjaxRequest)
import Network.HTTP.Affjax.Request as Request
import Network.HTTP.RequestHeader (RequestHeader(..))

--------------------------------------------------------------------------------
-- | generic builder
--------------------------------------------------------------------------------

type Decoder parsed decoded = {parse :: String -> Either String parsed, decode :: parsed -> Either String decoded}
type Encoder decoded parsed = {encode :: decoded -> parsed, print :: parsed -> Request.Request }

jsonDecoder :: forall a. DecodeJson a => Decoder Json a
jsonDecoder = {parse: jsonParser, decode: decodeJson}

jsonEncoder :: forall a. EncodeJson a => Encoder a Json
jsonEncoder = {encode: encodeJson, print: Request.Json}

data NoContent = NoContent
noContentDecoder :: Decoder String NoContent
noContentDecoder = {parse: pure, decode: \i -> if i == "" then Right NoContent else Left "Expected no content but got some."}
noContentEncoder :: Encoder NoContent String
noContentEncoder = {encode: const "", print: Request.String}

buildRequest_
  :: forall m route captures params headers body response parsed.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => RouteBuilder route captures params headers body response
  => (AffjaxRequest -> AffjaxRequest)
  -> RouteProxy route
  -> Captures captures
  -> QueryParams params
  -> Headers headers
  -> Decoder parsed response
  -> m response
buildRequest_ overRequest routeP captures params headers decoder = do
    {protocol, baseURL} <- ask
    let (SuspendedRoute route :: SuspendedRoute body response) = buildRoute routeP captures params headers
        reqHeaders = []
        affReq = overRequest defaultRequest
                   { method = route.requestMethod
                   , url = mkApiURL {protocol, baseURL} <> (joinWith "/" route.path) <> maybe "" ("?" <> _) route.queryString
                   , headers = defaultRequest.headers <> map (\(Tuple k v) -> RequestHeader k v) route.headers
                   }
    affResp <- affjax affReq
    getResult affReq decoder.parse decoder.decode affResp
  where
    mkApiURL {protocol, baseURL: url} = protocol <> ":" <> url


buildGetRequest
  :: forall m route captures params headers response parsed.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => RouteBuilder route captures params headers Void response
  => RouteProxy route
  -> Captures captures
  -> QueryParams params
  -> Headers headers
  -> Decoder parsed response
  -> m response
buildGetRequest routeP captures params headers decoder =
  buildRequest_ identity routeP captures params headers decoder

buildPostRequest
  :: forall m route captures params headers body response parsed parsed'.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => RouteBuilder route captures params headers body response
  => RouteProxy route
  -> Captures captures
  -> body
  -> QueryParams params
  -> Headers headers
  -> Decoder parsed response
  -> Encoder body parsed'
  -> m response
buildPostRequest routeP captures body params headers decoder encoder =
  buildRequest_ addContent routeP captures params headers decoder
  where
  addContent = _{content = pure $ encoder.print $ encoder.encode body}

buildDeleteRequest
  :: forall m route captures params headers body response parsed parsed'.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => RouteBuilder route captures params headers body response
  => RouteProxy route
  -> Captures captures
  -> body
  -> QueryParams params
  -> Headers headers
  -> Decoder parsed response
  -> Encoder body parsed'
  -> m response
buildDeleteRequest = buildPostRequest
