module Servant.API.PathComponent where

import Servant.API.Route (kind Route)

-- | A combinator to use for path components in a route, e.g. `/photos`
foreign import data S :: Symbol -> Route
