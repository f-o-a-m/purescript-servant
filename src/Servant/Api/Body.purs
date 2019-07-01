module Servant.API.Body (Body) where


import Servant.API.Route (kind Route)


foreign import data Body :: Type -> Type -> Route
