module Servant.Api.Types
  ( -- * Route Types
    kind Route
  , RouteCons
  , type (:>)
  , S
  , Capture
  , Body
  , QP
  , HDRS
  , GET
  , POST
  , DELETE
  , RouteProxy(..)
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
  , HeaderEntry(..)
  , noHeaders
  , class ToHeader
  , toHeader
  , class IsMethod
  , method
  , class MimeUnrender
  , mimeUnrender
  , class MimeRender
  , mimeRender
  ) where

import Prelude

import Affjax as Request
import Affjax.RequestBody as Request
import Affjax.ResponseFormat (ResponseFormat, json)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Array (cons, fromFoldable, intercalate)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList as RowList
import Record (delete, get)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Route types and kinds
--------------------------------------------------------------------------------

foreign import kind Route
foreign import data RouteCons :: Route -> Route -> Route
infixr 6 type RouteCons as :>

foreign import data S :: Symbol -> Route
foreign import data Capture :: Symbol -> Type -> Route
foreign import data QP :: #Type -> Route
foreign import data Body :: Type -> Type -> Route

foreign import data GET :: Type -> Type -> Route
foreign import data POST :: Type -> Type -> Route
foreign import data DELETE :: Type -> Type -> Route

foreign import data HDRS :: #Type -> Route


{-

type PostPhoto =
      S "album"
   :> Capture "albumID" Int
   :> Body Json Photo
   :> Header (authToken :: AuthToken)
   :> POST Json NoContent


type GetPhotos
   :> S "photo"
   :> QP ( postedBefore :: Maybe Date
         , postedBefore :: Maybe Date
         )
   :> Header (authToken :: AuthToken)
   :> Get Json (Array Photo)

-}

--class IsRequestBody a bodyType where
--  makeRequestBody :: a -> 

class IsMethod (r :: Route) where
  method :: RouteProxy r -> Method

instance isMethodGET :: IsMethod (GET ct a) where
  method _ = GET

instance isMethodPOST :: IsMethod (POST ct a) where
  method _ = POST

instance isMethodDELETE :: IsMethod (DELETE ct a) where
  method _ = DELETE

--------------------------------------------------------------------------------

class MimeUnrender ctype a where
  mimeUnrender :: Proxy a -> Proxy ctype -> { responseFormat :: ResponseFormat ctype
                                            , decode :: ctype -> Either String a
                                            }

instance mimeUnrenderJson :: DecodeJson a => MimeUnrender Json a where
  mimeUnrender _ _ = { responseFormat: json
                     , decode: decodeJson
                     }

--------------------------------------------------------------------------------

class MimeRender ctype a where
  mimeRender :: Proxy a -> Proxy ctype -> { print :: ctype -> Request.RequestBody
                                          , encode :: a -> ctype
                                          }

instance mimeRenderJson :: EncodeJson a => MimeRender Json a where
  mimeRender _ _ = { print: Request.json
                   , encode: encodeJson
                   }

--------------------------------------------------------------------------------
-- Route Building
--------------------------------------------------------------------------------

data RouteProxy (r :: Route) = RouteProxy

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

instance toCaptureInt :: ToCapture Int where
  toCapture = show

--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

newtype Headers r = Headers (Record r)

noHeaders :: Headers ()
noHeaders = Headers {}

class ToHeader a where
  toHeader :: a -> String

instance toHeaderString :: ToHeader String where
  toHeader = identity

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
