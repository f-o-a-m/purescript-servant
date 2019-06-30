module Servant.Client.Request where

import Prelude

import Affjax (ResponseFormatError(..))
import Affjax as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ignore)
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Exists (Exists, mkExists)
import Data.Lens ((.~))
import Data.Lens.Record (prop)
import Data.MediaType.Common (applicationJSON)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Error, catchError, error, message, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- | affjax utils
-------------------------------------------------------------------------------

newtype AjaxError' a = AjaxError
  { request :: Affjax.Request a
  , description :: ErrorDescription
  }

type AjaxError = Exists AjaxError'

data ErrorDescription = UnexpectedHTTPStatus StatusCode
                      | ParseError ResponseFormatError
                      | DecodingError String
                      | ConnectionError String

foreign import unsafeToString :: forall obj. obj -> String

errorToString :: AjaxError -> String
errorToString = unsafeToString

-- | Do an Affjax.affjax call but report Aff exceptions in our own MonadError
affjax :: forall m a
   . MonadError (AjaxError) m
  => MonadAff m
  => Affjax.Request a
  -> m (Affjax.Response a)
affjax req = do
   eRes <- liftAff (try $ Affjax.request req)
   case eRes of
     Left err -> throwError $ makeAjaxError req $ ConnectionError (message err)
     Right res -> case res.body of
       Right parsed -> pure $ res # prop (SProxy :: SProxy "body") .~ parsed
       Left formatErr -> throwError $ makeAjaxError req $ ParseError formatErr

getResult
  :: forall parsed decoded m.
     MonadError AjaxError m
  => Affjax.Request parsed
  -> (parsed -> Either String decoded)
  -> Affjax.Response parsed
  -> m decoded
getResult req decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  respBody <- if stCode >= 200 && stCode < 300
            then pure resp.body
            else throwError $ makeAjaxError req (UnexpectedHTTPStatus resp.status)
  case decode respBody of
    Left errMsg -> throwError $ makeAjaxError req (DecodingError errMsg)
    Right a -> pure a

makeAjaxError :: forall a. Affjax.Request a -> ErrorDescription -> AjaxError
makeAjaxError req desc = mkExists $
  AjaxError { request : req
            , description : desc
            }

newtype ClientEnv =
  ClientEnv { protocol :: String, baseURL :: String }

runRequest
  :: forall m a.
  MonadAff m
  => MonadThrow Error m
  => ClientEnv
  -> ReaderT ClientEnv (ExceptT AjaxError m) a
  -> m (Either AjaxError a)
runRequest apiUrl req = runExceptT $ runReaderT req apiUrl

assertRequest
  :: forall m a.
     MonadAff m
  => MonadThrow Error m
  => ClientEnv
  -> ReaderT ClientEnv (ExceptT AjaxError m) a
  -> m a
assertRequest apiSettings req = runRequest apiSettings req >>= either (throwError <<< error <<< errorToString) pure
