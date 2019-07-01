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

foreign import data CAP :: Symbol -> Type -> Route

newtype Capture (s :: Symbol) a = Capture a

capture :: forall s a. SProxy s -> a -> Capture s a
capture _ a = Capture a

uncapture :: forall s a. Capture s a -> a
uncapture (Capture a) = a

class ToCapture a where
  toCapture :: a -> String

instance stringToCapture :: ToCapture String where
  toCapture = identity

instance toCaptureInt :: ToCapture Int where
  toCapture = show
