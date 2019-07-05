module Servant.API.Capture
  ( CAP
  , Capture
  , capture
  , uncapture
  , class ToCapture
  , toCapture
  ) where

import Prelude

import Data.Symbol (SProxy)
import Servant.API.Route (kind Route)

-- | A combinator for route captures, for example "/photos/:photoID"
foreign import data CAP :: Symbol -> Type -> Route

-- | We use the 's :: Symbol' parameter because often times it's hard to remember
-- | the args are for nested captures.
newtype Capture (s :: Symbol) a = Capture a

capture :: forall s a. SProxy s -> a -> Capture s a
capture _ a = Capture a

uncapture :: forall s a. Capture s a -> a
uncapture (Capture a) = a

-- | Any type used as a capture must implement this typeclass
class ToCapture a where
  toCapture :: a -> String

instance stringToCapture :: ToCapture String where
  toCapture = identity

instance toCaptureInt :: ToCapture Int where
  toCapture = show
