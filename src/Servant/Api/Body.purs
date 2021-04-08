module Servant.API.Body (Body) where

import Servant.API.Route (kind Route)

-- | A combinator for request bodies. For example, 'Body Json PostPhotoBody'.
foreign import data Body :: Type -> Type -> Route
