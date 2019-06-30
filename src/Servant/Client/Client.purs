module Servant.Client.Client
  ( module Servant.Client.Client
  , module Reexport
  )where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Heterogeneous.Folding (class FoldlRecord, hfoldlWithIndex)
import Network.HTTP.Affjax (AffjaxRequest)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Request as Request
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prim.RowList (class RowToList)
import Servant.Api.Types (QueryParams(..), Captures(..), Required(..), Headers(..)) as Reexport
import Servant.Api.Types (class IsMethod, class MimeRender, class MimeUnrender, class ToCapture, type (:>), Body, Capture, Captures, GET, HDRS, HeaderEntry(..), Headers(..), QP, QueryParam, QueryParamEntry, QueryParams(..), Required(..), RouteProxy(..), S, formatQueryString, method, mimeRender, mimeUnrender, toCapture, kind Route)
import Servant.Client.Request (AjaxError(..), ClientEnv(..), affjax, defaultRequest, getResult)
import Type.Proxy (Proxy(..), Proxy2(..))

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


newtype SuspendedRoute =
  SuspendedRoute { queryString :: Maybe String
                 , path :: Array String
                 , body :: Maybe Request.Request
                 , headers :: Array (Tuple String String)
                 }

defaultSuspendedRoute :: SuspendedRoute
defaultSuspendedRoute = SuspendedRoute { queryString: Nothing
                                       , path: []
                                       , body: Nothing
                                       , headers: []
                                       }

class RunClient m where
  -- | How to make a request.
  runRequest :: Affjax.AffjaxRequest -> m (Affjax.AffjaxResponse String)

instance defaultRunClient :: (MonadAff m, MonadError AjaxError m) => RunClient m where
  runRequest = affjax

class HasClient (r :: Route) (m :: Type -> Type) f | m r -> f where
  buildClientRoute :: RouteProxy r -> Proxy2 m -> (SuspendedRoute -> f)

instance hasClientPathComponent
         :: ( HasClient r m f
            , IsSymbol s
            )
         => HasClient (S s :> r) m f where
  buildClientRoute (RouteProxy :: RouteProxy (S s :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) ->
    let pathComponent = reflectSymbol (SProxy :: SProxy s)
    in buildClientRoute (RouteProxy :: RouteProxy r) p $ SuspendedRoute route { path = cons pathComponent route.path}

else instance hasClientCapture
         :: ( HasClient r m f
            , IsSymbol s
            , ToCapture a
            )
         => HasClient (Capture s a :> r) m (a -> f) where
  buildClientRoute (RouteProxy :: RouteProxy (Capture s a :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) a  ->
    let pathComponent = toCapture a
    in buildClientRoute (RouteProxy :: RouteProxy r) p $ SuspendedRoute route { path = cons pathComponent route.path}

else instance hasClientBody
         :: ( HasClient r m f
            , MimeRender ct body
            )
         => HasClient (Body ct body :> r) m (body -> f) where
  buildClientRoute (RouteProxy :: RouteProxy (Body ct body :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) body ->
    let {print, encode} = mimeRender (Proxy :: Proxy body) (Proxy :: Proxy ct)
    in buildClientRoute (RouteProxy :: RouteProxy r) p $ SuspendedRoute route { body = Just $ print $ encode body }

else instance hasClientQPs
         :: ( HasClient r m f
            , RowToList params paramsList
            , FoldlRecord QueryParamEntry (Array QueryParam) paramsList params (Array QueryParam)
            )
         => HasClient (QP params :> r) m (QueryParams params -> f) where
  buildClientRoute (RouteProxy :: RouteProxy (QP params :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) params  ->
    buildClientRoute (RouteProxy :: RouteProxy r) p $ SuspendedRoute route { queryString = Just $ formatQueryString params }

else instance hasClientHeaders
         :: ( HasClient r m f
            , RowToList headers headersList
            , FoldlRecord HeaderEntry (Array (Tuple String String)) headersList headers (Array (Tuple String String))
            )
         => HasClient (HDRS headers :> r) m (Headers headers -> f) where
  buildClientRoute (RouteProxy :: RouteProxy (HDRS headers :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) (Headers hdrs)  ->
    buildClientRoute (RouteProxy :: RouteProxy r) p $ SuspendedRoute route { headers = hfoldlWithIndex HeaderEntry ([] :: Array (Tuple String String)) hdrs}

else instance hasClientVerb
         :: ( IsMethod (verb ct a)
            , MimeUnrender ct a
            , RunClient m
            , MonadAsk ClientEnv m
            , MonadAff m
            , MonadError AjaxError m
            )
         => HasClient (verb ct a) m (m a) where
  buildClientRoute r@(RouteProxy :: RouteProxy (verb ct a)) (Proxy2 :: Proxy2 m) = \(SuspendedRoute route) -> do
    ClientEnv clientEnv <- ask
    let apiURL = clientEnv.protocol <> ":" <> clientEnv.baseURL
        {parse, decode} = mimeUnrender (Proxy :: Proxy a) (Proxy :: Proxy ct)
        affReq = defaultRequest
          { method = Left $ method r
          , url = apiURL <> (joinWith "/" route.path) <> maybe "" ("?" <> _) route.queryString
          , headers = defaultRequest.headers <> map (\(Tuple k v) -> RequestHeader k v) route.headers
          }
    affResp <- runRequest affReq
    getResult affReq parse decode affResp



type TestGetR =
        S "photos"
     :> QP ( users :: Array Int
           , from :: Maybe String
           , to :: Required String
           )
     :> GET Json String

type ClientM = ReaderT ClientEnv (ExceptT AjaxError Aff)

testGet
  :: QueryParams ( users :: Array Int
                 , from :: Maybe String
                 , to :: Required String
                 )
  -> ClientM String
testGet = buildClientRoute (RouteProxy :: RouteProxy TestGetR) (Proxy2 :: Proxy2 ClientM) $ defaultSuspendedRoute

--buildRequest_
--  :: forall m route captures params headers body response parsed.
--     MonadAsk ClientEnv m
--  => MonadError AjaxError m
--  => MonadAff m
--  => RouteBuilder route captures params headers body response
--  => (AffjaxRequest -> AffjaxRequest)
--  -> RouteProxy route
--  -> Captures captures
--  -> QueryParams params
--  -> Headers headers
--  -> Decoder parsed response
--  -> m response
--buildRequest_ overRequest routeP captures params headers decoder = do
--    {protocol, baseURL} <- ask
--    let (SuspendedRoute route :: SuspendedRoute body response) = buildRoute routeP captures params headers
--        reqHeaders = []
--        affReq = overRequest defaultRequest
--                   { method = route.requestMethod
--                   , url = mkApiURL {protocol, baseURL} <> (joinWith "/" route.path) <> maybe "" ("?" <> _) route.queryString
--                   , headers = defaultRequest.headers <> map (\(Tuple k v) -> RequestHeader k v) route.headers
--                   }
--    affResp <- affjax affReq
--    getResult affReq decoder.parse decoder.decode affResp
--  where
--    mkApiURL {protocol, baseURL: url} = protocol <> ":" <> url
--
--
--buildGetRequest
--  :: forall m route captures params headers response parsed.
--     MonadAsk ClientEnv m
--  => MonadError AjaxError m
--  => MonadAff m
--  => RouteBuilder route captures params headers Void response
--  => RouteProxy route
--  -> Captures captures
--  -> QueryParams params
--  -> Headers headers
--  -> Decoder parsed response
--  -> m response
--buildGetRequest routeP captures params headers decoder =
--  buildRequest_ identity routeP captures params headers decoder
--
--buildPostRequest
--  :: forall m route captures params headers body response parsed parsed'.
--     MonadAsk ClientEnv m
--  => MonadError AjaxError m
--  => MonadAff m
--  => RouteBuilder route captures params headers body response
--  => RouteProxy route
--  -> Captures captures
--  -> body
--  -> QueryParams params
--  -> Headers headers
--  -> Decoder parsed response
--  -> Encoder body parsed'
--  -> m response
--buildPostRequest routeP captures body params headers decoder encoder =
--  buildRequest_ addContent routeP captures params headers decoder
--  where
--  addContent = _{content = pure $ encoder.print $ encoder.encode body}
--
--buildDeleteRequest
--  :: forall m route captures params headers body response parsed parsed'.
--     MonadAsk ClientEnv m
--  => MonadError AjaxError m
--  => MonadAff m
--  => RouteBuilder route captures params headers body response
--  => RouteProxy route
--  -> Captures captures
--  -> body
--  -> QueryParams params
--  -> Headers headers
--  -> Decoder parsed response
--  -> Encoder body parsed'
--  -> m response
--buildDeleteRequest = buildPostRequest
