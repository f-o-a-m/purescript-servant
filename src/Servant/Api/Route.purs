module Servant.API.Route
  ( kind Route
  , RouteProxy(..)
  , RouteCons
  , type (:>)
  ) where

foreign import kind Route

data RouteProxy (r :: Route)
  = RouteProxy

foreign import data RouteCons :: Route -> Route -> Route

infixr 6 type RouteCons as :>
