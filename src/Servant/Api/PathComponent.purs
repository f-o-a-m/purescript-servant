module Servant.API.PathComponent where


import Servant.API.Route (kind Route)


foreign import data S :: Symbol -> Route
