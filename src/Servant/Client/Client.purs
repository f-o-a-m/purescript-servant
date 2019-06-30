module Servant.Client.Client
  ( module Servant.Client.Client
  , module Reexport
  )where

import Prelude

import Affjax as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged, untagged)
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Heterogeneous.Folding (class FoldlRecord, hfoldlWithIndex)
import Prim.RowList (class RowToList)
import Servant.Api.Types (QueryParams(..), Captures(..), Required(..), Headers(..)) as Reexport
import Servant.Api.Types (class IsMethod, class MimeRender, class MimeUnrender, class ToCapture, type (:>), Body, Capture, Captures, GET, HDRS, HeaderEntry(..), Headers(..), POST, QP, QueryParam, QueryParamEntry, QueryParams(..), Required(..), RouteProxy(..), S, formatQueryString, method, mimeRender, mimeUnrender, toCapture, kind Route)
import Servant.Client.Request (AjaxError(..), ClientEnv(..), affjax, getResult)
import Type.Proxy (Proxy(..), Proxy2(..))

--------------------------------------------------------------------------------
-- | generic builder
--------------------------------------------------------------------------------

data NoContent = NoContent

instance decodeNoContent :: DecodeJson NoContent where
  decodeJson = const $ pure NoContent


newtype SuspendedRoute =
  SuspendedRoute { queryString :: Maybe String
                 , path :: Array String
                 , body :: Maybe RequestBody
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
  runRequest :: forall a. Affjax.Request a -> m (Affjax.Response a)

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
         => HasClient (Capture s a :> r) m (Tagged (SProxy s) a -> f) where
  buildClientRoute (RouteProxy :: RouteProxy (Capture s a :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) a ->
    let pathComponent = toCapture $ untagged a
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
        {responseFormat, decode} = mimeUnrender (Proxy :: Proxy a) (Proxy :: Proxy ct)
        affReq =
          { method: Left $ method r
          , url: apiURL <> (joinWith "/" route.path) <> maybe "" ("?" <> _) route.queryString
          , headers: map (\(Tuple k v) -> RequestHeader k v) route.headers
          , content: route.body
          , responseFormat
          , password: Nothing
          , username: Nothing
          , withCredentials: false
          }
    affResp <- runRequest affReq
    getResult affReq decode affResp

type ClientM = ReaderT ClientEnv (ExceptT AjaxError Aff)

type TestGetR =
  S "photos"
  :> QP ( users :: Array Int
        , from :: Maybe String
        , to :: Required String
        )
  :> GET Json String

testGet
  :: QueryParams ( users :: Array Int
                 , from :: Maybe String
                 , to :: Required String
                 )
  -> ClientM String
testGet = buildClientRoute (RouteProxy :: RouteProxy TestGetR) (Proxy2 :: Proxy2 ClientM) $ defaultSuspendedRoute

type TestPostR =
     S "photos"
  :> Capture "userID" Int
  :> Body Json String
  :> HDRS ("AuthToken" :: String)
  :> POST Json NoContent

testPost
  :: Tagged (SProxy "userID") Int
  -> String
  -> Headers ("AuthToken" :: String)
  -> ClientM NoContent
testPost = buildClientRoute (RouteProxy :: RouteProxy TestPostR) (Proxy2 :: Proxy2 ClientM) $ defaultSuspendedRoute
