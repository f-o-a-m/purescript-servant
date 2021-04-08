module Servant.Client.Request
  ( ClientEnv(..)
  , class RunRequest
  , runRequest
  , defaultRunRequest
  , parseResult
  ) where

import Prelude

import Affjax (printError)
import Affjax as Affjax
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Servant.Client.Error (AjaxError, ErrorDescription(..), makeAjaxError)

newtype ClientEnv =
  ClientEnv { protocol :: String, baseURL :: String }

class RunRequest m where
  runRequest :: forall a. Affjax.Request a -> m (Affjax.Response a)

-- | Do an Affjax.affjax call but report Aff exceptions in our own MonadError
defaultRunRequest :: forall m a
   . MonadError (AjaxError) m
  => MonadAff m
  => Affjax.Request a
  -> m (Affjax.Response a)
defaultRunRequest req = do
  eRes <- liftAff (Affjax.request req)
  case eRes of
    Right res -> pure res
    Left err -> throwError $ makeAjaxError req $ RequestError (printError err)

-- | Parse the result from the respose body using the decoder. At this point the
-- | response body has already been parsed into the 'ResponseFormat' type, e.g. Json,
-- | and we are decoding from that.
parseResult
  :: forall parsed decoded m.
     MonadError AjaxError m
  => Affjax.Request parsed
  -> (parsed -> Either String decoded)
  -> Affjax.Response parsed
  -> m decoded
parseResult req decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  respBody <- if stCode >= 200 && stCode < 300
            then pure resp.body
            else throwError $ makeAjaxError req (UnexpectedHTTPStatus resp.status)
  case decode respBody of
    Left errMsg -> throwError $ makeAjaxError req (DecodingError errMsg)
    Right a -> pure a
