module Servant.API.Header
  ( HDRs
  , Headers(..)
  , class ToHeader
  , toHeader
  , HeaderEntry
  , foldHeaders
  ) where

import Prelude

import Data.Array (cons)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, hfoldlWithIndex)
import Prim.RowList (class RowToList)
import Servant.API.Route (kind Route)

-- | A combinator for request headers
foreign import data HDRs :: #Type -> Route

newtype Headers r = Headers (Record r)

-- | Any type you want to use as a request header value must implement this typeclass.
class ToHeader a where
  toHeader :: a -> String

instance toHeaderString :: ToHeader String where
  toHeader = identity

data HeaderEntry = HeaderEntry

instance headerEntry :: (ToHeader a, IsSymbol sym) => FoldingWithIndex HeaderEntry (SProxy sym) (Array (Tuple String String)) a (Array (Tuple String String)) where
  foldingWithIndex HeaderEntry prop acc a = Tuple (reflectSymbol prop) (toHeader a) `cons` acc

foldHeaders
  :: forall hd hdList.
     RowToList hd hdList
  => FoldlRecord HeaderEntry (Array (Tuple String String)) hdList hd (Array (Tuple String String))
  => Headers hd
  -> Array (Tuple String String)
foldHeaders (Headers hdrs) = hfoldlWithIndex HeaderEntry ([] :: Array (Tuple String String)) (hdrs :: Record hd)
