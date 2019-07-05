module Servant.API.QueryParam
  ( QPs
  , QueryParams(..)
  , Required(..)
  , QueryParam
  , class QueryParam1
  , queryParam1
  , QueryParamEntry
  , formatQueryString
  , class EncodeQueryParam
  , encodeQueryParam
  ) where

import Prelude

import Data.Array (cons, fromFoldable, intercalate)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, hfoldlWithIndex)
import Prim.RowList (class RowToList)
import Servant.API.Route (kind Route)

-- | A combinator to use for query parameters. Query parameter values can use one of
-- | three functors in order to specify what kind of parameter it is:
-- | 'Maybe' for optional, 'Required' for required, 'Array' for multi-valued params.
foreign import data QPs :: #Type -> Route

newtype QueryParams r = QueryParams (Record r)

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

type QueryString = String

formatQueryString
  :: forall qp qpList.
     RowToList qp qpList
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
