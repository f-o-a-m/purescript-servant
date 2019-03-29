module Servant.Api.Types
  ( -- * Route Types
    kind Route
  , RouteCons
  , type (:>)
  , S
  , Capture
  , QP
  , HDRS
  , GET
  , POST
  , DELETE
  , RouteProxy(..)
  , SuspendedRoute(..)
  , class RouteBuilder
  , buildRoute
    -- * Captures
  , Captures(..)
  , noCaptures
    -- * Query Params
  , class EncodeQueryParam
  , encodeQueryParam
  , QueryParam
  , Required(..)
  , QueryParams(..)
  , noQueryParams
  , class QueryParam1
  , queryParam1
  , class RecToQueryParams
  , recToQueryParams
  , formatQueryString
  , class RecToHeaders
  , recToHeaders
  , Headers(..)
  , noHeaders
  , class ToHeader
  , toHeader
  ) where

import Prelude

import Data.Array (cons, fromFoldable, intercalate)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RowList
import Record (delete, get)
import Type.Data.RowList as TRowList

--------------------------------------------------------------------------------
-- Route types and kinds
--------------------------------------------------------------------------------

foreign import kind Route
foreign import data RouteCons :: Route -> Route -> Route
infixr 6 type RouteCons as :>

foreign import data S :: Symbol -> Route
foreign import data Capture :: Symbol -> Type -> Route
foreign import data QP :: #Type -> Route

foreign import data GET :: Type -> Route
foreign import data POST :: Type -> Type -> Route
foreign import data DELETE :: Type -> Type -> Route

foreign import data HDRS :: #Type -> Route

--------------------------------------------------------------------------------
-- Route Building
--------------------------------------------------------------------------------

newtype SuspendedRoute body response =
  SuspendedRoute { queryString :: Maybe String
                 , path :: Array String
                 , headers :: Array (Tuple String String)
                 , requestMethod :: Either Method CustomMethod
                 }

data RouteProxy (r :: Route) = RouteProxy

class RouteBuilder (r :: Route) (captures :: #Type) (params :: #Type) (headers :: #Type) body response | r -> body, r -> response where
  buildRoute :: RouteProxy r -> Captures captures -> QueryParams params -> Headers headers -> SuspendedRoute body response


instance baseRouteBuilderGetNoParams ::
  ( RowList.RowToList captures RowList.Nil
  , RowList.RowToList params RowList.Nil
  , RowList.RowToList headers RowList.Nil
  ) => RouteBuilder (GET response) captures params headers Void response where
  buildRoute _ _ _ _ = SuspendedRoute { queryString: Nothing
                                      , path: []
                                      , headers: []
                                      , requestMethod: Left GET
                                      }

instance baseRouteBuilderPost ::
  ( RowList.RowToList captures RowList.Nil
  , RowList.RowToList params lp
  , RecToQueryParams lp params
  , RowList.RowToList headers lh
  , RecToQueryParams lh headers
  ) => RouteBuilder (POST body response) captures params headers body response where
  buildRoute _ _ _ _ = SuspendedRoute { queryString: Nothing
                                      , path: []
                                      , headers: []
                                      , requestMethod: Left POST
                                      }
instance baseRouteBuilderDelete ::
  ( RowList.RowToList captures RowList.Nil
  , RowList.RowToList params l
  , RecToQueryParams l params
  , RowList.RowToList headers lh
  , RecToQueryParams lh headers
  ) => RouteBuilder (DELETE body response) captures params headers body response where
  buildRoute _ _ _ _ = SuspendedRoute { queryString: Nothing
                                      , path: []
                                      , headers: []
                                      , requestMethod: Left DELETE
                                      }

instance baseRouteBuilderParams ::
  ( RowList.RowToList params l
  , RecToQueryParams l params
  , RouteBuilder r captures () headers body response
  ) => RouteBuilder (QP params :> r) captures params headers body response where
  buildRoute _ captures params headers =
    let SuspendedRoute route = buildRoute (RouteProxy :: RouteProxy r) captures (QueryParams {}) headers
    in SuspendedRoute route { queryString = Just $ formatQueryString params }

instance baseRouteBuilderHeaders ::
  ( RowList.RowToList headers l
  , RecToHeaders l headers
  , RouteBuilder r captures params () body response
  ) => RouteBuilder (HDRS headers :> r) captures params headers body response where
  buildRoute _ captures params (Headers hdrs) =
    let SuspendedRoute route = buildRoute (RouteProxy :: RouteProxy r) captures params (Headers {})
    in SuspendedRoute route { headers = recToHeaders (TRowList.RLProxy :: TRowList.RLProxy l) hdrs}

instance stringRouteBuilder ::
  ( IsSymbol s
  , RouteBuilder r captures params headers body response
  ) => RouteBuilder (S s :> r) captures params headers body response where
  buildRoute _ cs params headers =
    let pathComponent = reflectSymbol (SProxy :: SProxy s)
        SuspendedRoute route = buildRoute (RouteProxy :: RouteProxy r) cs params headers
    in SuspendedRoute route { path = cons pathComponent route.path}

instance captureRouteBuilder ::
  ( IsSymbol s
  , RouteBuilder r cs params headers body response
  , Row.Cons s c cs as
  , Row.Lacks s cs
  , ToCapture c
  ) => RouteBuilder (Capture s c :> r) as params headers body response where
  buildRoute _ (Captures captures) params headers =
    let c = toCapture $ get (SProxy :: SProxy s) captures
        (cs :: Record cs) = delete (SProxy :: SProxy s) captures
        SuspendedRoute route = buildRoute (RouteProxy :: RouteProxy r) (Captures cs) params headers
    in SuspendedRoute route {path = cons c route.path}

--------------------------------------------------------------------------------
-- | Captures
--------------------------------------------------------------------------------

class ToCapture a where
  toCapture :: a -> String

instance stringToCapture :: ToCapture String where
  toCapture = identity

newtype Captures r = Captures (Record r)

noCaptures :: Captures ()
noCaptures = Captures {}


--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

newtype Headers r = Headers (Record r)

class RecToHeaders (l :: RowList.RowList) r | l -> r where
  recToHeaders :: TRowList.RLProxy l -> Record r -> Array (Tuple String String)

instance recToHeadersBase :: RecToHeaders RowList.Nil r where
  recToHeaders _ _ = []

instance recToHeadersInductive
         :: ( IsSymbol s
            , Row.Cons s a r' r
            , Row.Lacks s r'
            , RecToHeaders l r'
            , ToHeader a
            ) => RecToHeaders (RowList.Cons s a l) r where
           recToHeaders _ r =
             let k = reflectSymbol (SProxy :: SProxy s)
                 (hdr :: a) = get (SProxy :: SProxy s) r
                 (r' :: Record r') = delete (SProxy :: SProxy s) r
                 rest = recToHeaders (TRowList.RLProxy :: TRowList.RLProxy l) r'
             in Tuple k (toHeader hdr) `cons` rest

noHeaders :: Headers ()
noHeaders = Headers {}

class ToHeader a where
  toHeader :: a -> String

--------------------------------------------------------------------------------
-- Query Params
--------------------------------------------------------------------------------

type QueryString = String

newtype QueryParams r = QueryParams (Record r)

noQueryParams :: QueryParams ()
noQueryParams = QueryParams {}

newtype Required a = Required a

data QueryParam =
    SingleParam String String
  | ArrayParams String (NEA.NonEmptyArray String)

class QueryParam1 f where
  queryParam1 :: forall a. EncodeQueryParam a => String -> f a -> Maybe QueryParam

instance queryParam1Required :: QueryParam1 Required where
  queryParam1 s (Required a) =  Just $ SingleParam s $ encodeQueryParam a

instance queryParam1Maybe :: QueryParam1 Maybe where
  queryParam1 s = map \a -> SingleParam s $ encodeQueryParam a

instance queryParam1Array :: QueryParam1 Array where
  queryParam1 s as = ArrayParams s <$> NEA.fromArray (map encodeQueryParam as)

class RecToQueryParams (l :: RowList.RowList) r | l -> r where
  recToQueryParams :: TRowList.RLProxy l -> Record r -> List QueryParam

instance recToQueryParamsBase :: RecToQueryParams RowList.Nil r where
  recToQueryParams _ _ = Nil

instance recToQueryParamsInductive
  :: ( IsSymbol s
     , Row.Cons s (f a) r' r
     , Row.Lacks s r'
     , RecToQueryParams l r'
     , QueryParam1 f
     , EncodeQueryParam a
     ) => RecToQueryParams (RowList.Cons s (f a) l) r where
  recToQueryParams _ r =
    let k = reflectSymbol (SProxy :: SProxy s)
        (as :: f a) = get (SProxy :: SProxy s) r
        (r' :: Record r') = delete (SProxy :: SProxy s) r
        rest = recToQueryParams (TRowList.RLProxy :: TRowList.RLProxy l) r'
    in maybe rest (\a -> Cons a rest) $ queryParam1 k as

formatQueryString
  :: forall l r.
     RowList.RowToList r l
  => RecToQueryParams l r
  => QueryParams r
  -> QueryString
formatQueryString (QueryParams r) =
    let paramsList = recToQueryParams (TRowList.RLProxy :: TRowList.RLProxy l) r
    in joinWith "&" <<< fromFoldable $ map formatParam paramsList
  where
    formatParam :: QueryParam -> String
    formatParam qp = case qp of
      SingleParam k v -> k <> "=" <> v
      ArrayParams k vs -> intercalate "&" $ map (\v -> k <> "=" <> v) vs

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

class EncodeQueryParam a where
  encodeQueryParam :: a -> String

instance encodeQueryParamBoolean :: EncodeQueryParam Boolean where
  encodeQueryParam = show

instance encodeQueryParamNumber :: EncodeQueryParam Number where
  encodeQueryParam = show

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam = show

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam = identity
