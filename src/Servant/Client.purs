module Servant.Client
  ( makeClientRoute
  , module Servant.Client.Error
  , module Servant.Client.Request
  ) where

import Control.Monad.Except (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Servant.API as API
import Servant.Client.Error (AjaxError, AjaxError', ErrorDescription(..), errorToString, makeAjaxError)
import Servant.Client.Request (ClientEnv(..))
import Servant.Client.HasClient (class HasClient, buildClientRoute, defaultSuspendedRoute)
import Type.Proxy (Proxy2(..))

makeClientRoute
  :: forall m r f.
     HasClient r m f
  => MonadAff m
  => MonadError AjaxError m
  => API.RouteProxy r
  -> f
makeClientRoute r =
  buildClientRoute r (Proxy2 :: Proxy2 m) defaultSuspendedRoute
