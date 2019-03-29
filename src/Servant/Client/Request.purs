module Servant.Client.Request where

import Prelude

import Effect.Aff (Aff, Error, catchError, message, error, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadThrow, class MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Response as Response
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.RequestHeader (RequestHeader(..))

--------------------------------------------------------------------------------
-- | affjax utils
-------------------------------------------------------------------------------

newtype AjaxError = AjaxError
  { request :: Affjax.AffjaxRequest
  , description :: ErrorDescription
  }

data ErrorDescription = UnexpectedHTTPStatus (Affjax.AffjaxResponse String)
                      | ParsingError String
                      | DecodingError String
                      | ConnectionError String

foreign import unsafeToString :: forall obj. obj -> String

errorToString :: AjaxError -> String
errorToString = unsafeToString

defaultRequest :: Affjax.AffjaxRequest
defaultRequest = Affjax.defaultRequest
  { headers =
      [ Accept applicationJSON
      , ContentType applicationJSON
      ]
  }

-- | Do an Affjax.affjax call but report Aff exceptions in our own MonadError
affjax :: forall m
   . MonadError (AjaxError) m
  => MonadAff m
  => Affjax.AffjaxRequest
  -> m (Affjax.AffjaxResponse String)
affjax req = liftWithError $ Affjax.affjax Response.string req
  where
    liftWithError :: forall a. Aff a -> m a
    liftWithError action = do
      res <- liftAff $ toEither action
      toAjaxError res

    toEither :: forall a. Aff a -> Aff (Either String a)
    toEither action = catchError (Right <$> action) $ \e ->
      pure $ Left (message e)

    toAjaxError :: forall a. Either String a -> m a
    toAjaxError r = case r of
        Left err -> throwError $ makeAjaxError req $ ConnectionError err
        Right v  -> pure v

getResult
  :: forall parsed decoded m.
     MonadError AjaxError m
  => Affjax.AffjaxRequest
  -> (String -> Either String parsed)
  -> (parsed -> Either String decoded)
  -> Affjax.AffjaxResponse String -> m decoded
getResult req' parse decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then pure resp.response
            else throwError $ makeAjaxError req' (UnexpectedHTTPStatus resp)
  jVal <- throwLeft <<< lmap (reportRequestError req' ParsingError fVal) <<< parse $ fVal
  throwLeft <<< lmap (reportRequestError req' DecodingError fVal) <<< decode $ jVal
  where
    throwLeft :: forall a' e' m'. MonadError e' m' => Either e' a' -> m' a'
    throwLeft (Left e) = throwError e
    throwLeft (Right a) = pure a

    reportRequestError :: Affjax.AffjaxRequest -> (String -> ErrorDescription) -> String -> String -> AjaxError
    reportRequestError req'' err source msg = makeAjaxError req'' $ reportError err source msg

    reportError :: forall err. (String -> err) -> String -> String  -> err
    reportError err source msg = err $ msg <> ", source: '" <> source <> "'"

makeAjaxError :: Affjax.AffjaxRequest -> ErrorDescription -> AjaxError
makeAjaxError req desc = AjaxError { request : req
                                   , description : desc
                                   }

type ClientEnv = { protocol :: String, baseURL :: String }

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
