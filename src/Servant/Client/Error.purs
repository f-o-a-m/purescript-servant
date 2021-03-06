module Servant.Client.Error
  ( AjaxError'
  , AjaxError
  , ErrorDescription(..)
  , errorDescription
  , makeAjaxError
  , errorToString
  ) where

import Prelude
import Affjax as Affjax
import Affjax.StatusCode (StatusCode)
import Data.Exists (Exists, mkExists, runExists)
import Data.Lens (Lens', lens)

newtype AjaxError' a
  = AjaxError'
  { request :: Affjax.Request a
  , description :: ErrorDescription
  }

newtype AjaxError
  = AjaxError (Exists AjaxError')

unAjaxError :: AjaxError -> Exists AjaxError'
unAjaxError (AjaxError e) = e

errorDescription :: Lens' AjaxError ErrorDescription
errorDescription =
  lens (unAjaxError >>> runExists \(AjaxError' { description }) -> description)
    (\(AjaxError err) d -> runExists (\(AjaxError' ae) -> AjaxError $ mkExists $ AjaxError' ae { description = d }) err)

data ErrorDescription
  = UnexpectedHTTPStatus StatusCode
  | DecodingError String
  | RequestError String

instance showErrorDecription :: Show ErrorDescription where
  show e = case e of
    UnexpectedHTTPStatus status -> "UnexpectedHTTPStatus " <> show status
    DecodingError e' -> "DecodingError " <> e'
    RequestError e' -> "RequestError " <> e'

makeAjaxError :: forall a. Affjax.Request a -> ErrorDescription -> AjaxError
makeAjaxError req desc =
  AjaxError <<< mkExists
    $ AjaxError'
        { request: req
        , description: desc
        }

foreign import unsafeToString :: forall obj. obj -> String

errorToString :: AjaxError -> String
errorToString = unsafeToString
