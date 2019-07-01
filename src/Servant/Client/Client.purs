module Servant.Client.Client where

import Prelude

import Affjax as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Heterogeneous.Folding (class FoldlRecord)
import Prim.RowList (class RowToList)
import Servant.API.Types as API
import Servant.API.Types (type (:>))
import Servant.Client.Error (AjaxError)
import Servant.Client.Request (ClientEnv(..), parseResult)
import Servant.Client.Request as ClientRequest
import Type.Proxy (Proxy(..), Proxy2(..))

--------------------------------------------------------------------------------
-- | generic builder
--------------------------------------------------------------------------------

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
  runRequest = ClientRequest.runRequest

class HasClient (r :: API.Route) (m :: Type -> Type) f | m r -> f where
  buildClientRoute :: API.RouteProxy r -> Proxy2 m -> (SuspendedRoute -> f)

instance hasClientPathComponent
         :: ( HasClient r m f
            , IsSymbol s
            )
         => HasClient (API.S s :> r) m f where
  buildClientRoute (API.RouteProxy :: API.RouteProxy (API.S s :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) ->
    let pathComponent = reflectSymbol (SProxy :: SProxy s)
    in buildClientRoute (API.RouteProxy :: API.RouteProxy r) p $ SuspendedRoute route { path = cons pathComponent route.path}

else instance hasClientCapture
         :: ( HasClient r m f
            , IsSymbol s
            , API.ToCapture a
            )
         => HasClient (API.CAP s a :> r) m (API.Capture s a -> f) where
  buildClientRoute (API.RouteProxy :: API.RouteProxy (API.CAP s a :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) a ->
    let pathComponent = API.toCapture $ API.uncapture a
    in buildClientRoute (API.RouteProxy :: API.RouteProxy r) p $ SuspendedRoute route { path = cons pathComponent route.path}

else instance hasClientBody
         :: ( HasClient r m f
            , API.MimeRender ct body
            )
         => HasClient (API.Body ct body :> r) m (body -> f) where
  buildClientRoute (API.RouteProxy :: API.RouteProxy (API.Body ct body :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) body ->
    let {print, encode} = API.mimeRender (Proxy :: Proxy body) (Proxy :: Proxy ct)
    in buildClientRoute (API.RouteProxy :: API.RouteProxy r) p $ SuspendedRoute route { body = Just $ print $ encode body }

else instance hasClientQPs
         :: ( HasClient r m f
            , RowToList params paramsList
            , FoldlRecord API.QueryParamEntry (Array API.QueryParam) paramsList params (Array API.QueryParam)
            )
         => HasClient (API.QPs params :> r) m (API.QueryParams params -> f) where
  buildClientRoute (API.RouteProxy :: API.RouteProxy (API.QPs params :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) params  ->
    buildClientRoute (API.RouteProxy :: API.RouteProxy r) p $ SuspendedRoute route { queryString = Just $ API.formatQueryString params }

else instance hasClientHeaders
         :: ( HasClient r m f
            , RowToList headers headersList
            , FoldlRecord API.HeaderEntry (Array (Tuple String String)) headersList headers (Array (Tuple String String))
            )
         => HasClient (API.HDRS headers :> r) m (API.Headers headers -> f) where
  buildClientRoute (API.RouteProxy :: API.RouteProxy (API.HDRS headers :> r)) p@(Proxy2 :: Proxy2 m) = \(SuspendedRoute route) hdrs  ->
    buildClientRoute (API.RouteProxy :: API.RouteProxy r) p $ SuspendedRoute route { headers = API.foldHeaders hdrs}

else instance hasClientVerb
         :: ( API.IsMethod (verb ct a)
            , API.MimeUnrender ct a
            , RunClient m
            , MonadAsk ClientEnv m
            , MonadAff m
            , MonadError AjaxError m
            )
         => HasClient (verb ct a) m (m a) where
  buildClientRoute r@(API.RouteProxy :: API.RouteProxy (verb ct a)) (Proxy2 :: Proxy2 m) = \(SuspendedRoute route) -> do
    ClientEnv clientEnv <- ask
    let apiURL = clientEnv.protocol <> ":" <> clientEnv.baseURL
        {responseFormat, decode} = API.mimeUnrender (Proxy :: Proxy a) (Proxy :: Proxy ct)
        affReq =
          { method: Left $ API.method r
          , url: apiURL <> (joinWith "/" route.path) <> maybe "" ("?" <> _) route.queryString
          , headers: map (\(Tuple k v) -> RequestHeader k v) route.headers
          , content: route.body
          , responseFormat
          , password: Nothing
          , username: Nothing
          , withCredentials: false
          }
    affResp <- runRequest affReq
    parseResult affReq decode affResp

{-
type ClientM = ReaderT ClientEnv (ExceptT AjaxError Aff)

type TestGetR =
  S "photos"
  :> QPs ( users :: Array Int
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
-}
