module Servant.Client.Error
  ( AjaxError'
  , AjaxError
  , ErrorDescription(..)
  , makeAjaxError
  , errorToString
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.StatusCode (StatusCode)
import Data.Exists (Exists, mkExists, runExists)
import Data.Lens (Lens', lens)

newtype AjaxError' a =
  AjaxError { request :: Affjax.Request a
            , description :: ErrorDescription
            }

errorDescription :: Lens' AjaxError ErrorDescription
errorDescription =
  lens (runExists \(AjaxError {description}) -> description)
    (\err d -> runExists (\(AjaxError ae) -> mkExists $ AjaxError ae {description = d}) err)

type AjaxError = Exists AjaxError'

data ErrorDescription =
    UnexpectedHTTPStatus StatusCode
  | ParseError ResponseFormatError
  | DecodingError String
  | ConnectionError String

makeAjaxError :: forall a. Affjax.Request a -> ErrorDescription -> AjaxError
makeAjaxError req desc = mkExists $
  AjaxError { request : req
            , description : desc
            }

foreign import unsafeToString :: forall obj. obj -> String

errorToString :: AjaxError -> String
errorToString = unsafeToString
