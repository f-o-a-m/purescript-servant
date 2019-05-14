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
  , class ToCapture
  , toCapture
  , Captures(..)
  , noCaptures
    -- * Query Params
  , class EncodeQueryParam
  , encodeQueryParam
  , QueryParam
  , Required(..)
  , QueryParams(..)
  , noQueryParams
  , QueryParamEntry
  , class QueryParam1
  , queryParam1
  , formatQueryString
  , Headers(..)
  , HeaderEntry
  , noHeaders
  , class ToHeader
  , toHeader
  ) where

import Prelude

import Data.Array (cons, fromFoldable, intercalate)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList as RowList
import Record (delete, get)

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


instance baseRouteBuilderGetNoParams :: RouteBuilder (GET response) captures params headers Void response where
  buildRoute _ _ _ _ = SuspendedRoute { queryString: Nothing
                                      , path: []
                                      , headers: []
                                      , requestMethod: Left GET
                                      }

instance baseRouteBuilderPost :: RouteBuilder (POST body response) captures params headers body response where
  buildRoute _ _ _ _ = SuspendedRoute { queryString: Nothing
                                      , path: []
                                      , headers: []
                                      , requestMethod: Left POST
                                      }
instance baseRouteBuilderDelete :: RouteBuilder (DELETE body response) captures params headers body response where
  buildRoute _ _ _ _ = SuspendedRoute { queryString: Nothing
                                      , path: []
                                      , headers: []
                                      , requestMethod: Left DELETE
                                      }

instance baseRouteBuilderParams ::
  ( RouteBuilder r captures () headers body response
  , RowList.RowToList params paramsList
  , FoldlRecord QueryParamEntry (Array QueryParam) paramsList params (Array QueryParam)
  ) => RouteBuilder (QP params :> r) captures params headers body response where
  buildRoute _ captures params headers =
    let SuspendedRoute route = buildRoute (RouteProxy :: RouteProxy r) captures noQueryParams headers
    in SuspendedRoute route { queryString = Just $ formatQueryString params }

instance baseRouteBuilderHeaders ::
  ( RouteBuilder r captures params () body response
  , RowList.RowToList headers headersList
  , FoldlRecord HeaderEntry (Array (Tuple String String)) headersList headers (Array (Tuple String String))
  ) => RouteBuilder (HDRS headers :> r) captures params headers body response where
  buildRoute _ captures params (Headers hdrs) =
    let SuspendedRoute route = buildRoute (RouteProxy :: RouteProxy r) captures params noHeaders
    in SuspendedRoute route { headers = hfoldlWithIndex HeaderEntry ([] :: Array (Tuple String String)) hdrs}

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

noHeaders :: Headers ()
noHeaders = Headers {}

class ToHeader a where
  toHeader :: a -> String

data HeaderEntry = HeaderEntry

instance headerEntry :: (ToHeader a, IsSymbol sym) => FoldingWithIndex HeaderEntry (SProxy sym) (Array (Tuple String String)) a (Array (Tuple String String)) where
  foldingWithIndex HeaderEntry prop acc a = Tuple (reflectSymbol prop) (toHeader a) `cons` acc

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

data QueryParamEntry = QueryParamEntry

instance queryParamEntry :: (QueryParam1 f, EncodeQueryParam a, IsSymbol sym) => FoldingWithIndex QueryParamEntry (SProxy sym) (Array QueryParam) (f a) (Array QueryParam) where
  foldingWithIndex QueryParamEntry prop acc f = maybe acc (\param -> param `cons` acc) $ queryParam1 (reflectSymbol prop) f

formatQueryString
  :: forall qp qpList.
     RowList.RowToList qp qpList
  => FoldlRecord QueryParamEntry (Array QueryParam) qpList qp (Array QueryParam)
  => QueryParams qp
  -> QueryString
formatQueryString (QueryParams r) =
    let paramsList = hfoldlWithIndex QueryParamEntry ([] :: Array QueryParam) (r :: Record qp)
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
