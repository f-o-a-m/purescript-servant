module Servant.API.Method
  ( GET
  , POST
  , DELETE
  , class IsMethod
  , method
  ) where

import Data.HTTP.Method (Method(..))
import Servant.API.Route (RouteProxy, kind Route)

foreign import data GET :: Type -> Type -> Route

foreign import data POST :: Type -> Type -> Route

foreign import data DELETE :: Type -> Type -> Route

class IsMethod (r :: Route) where
  method :: RouteProxy r -> Method

instance isMethodGET :: IsMethod (GET ct a) where
  method _ = GET

instance isMethodPOST :: IsMethod (POST ct a) where
  method _ = POST

instance isMethodDELETE :: IsMethod (DELETE ct a) where
  method _ = DELETE
