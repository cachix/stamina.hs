module Stamina.HTTP (retry, handler) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, fromException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime, defaultTimeLocale, readPTime, rfc822DateFormat, secondsToNominalDiffTime)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (statusIsServerError, tooManyRequests429)
import Stamina qualified
import Text.Read (Read (readPrec), readMaybe)
import Text.Read qualified as ReadPrec

-- | Retry handler for HTTP requests.
--
-- Retries a subset of HTTP exceptions and overrides the delay with the Retry-After header if present.
retry :: (MonadIO m, MonadCatch m) => Stamina.RetrySettings -> (Stamina.RetryStatus -> m a) -> m a
retry settings = Stamina.retryOnExceptions settings handler

handler :: (MonadIO m) => SomeException -> m Stamina.RetryAction
handler =
  httpExceptionToRetryAction . fromException
  where
    -- httpExceptionToRetryAction :: Maybe HTTP.HttpException -> m Stamina.RetryAction
    httpExceptionToRetryAction (Just exc@(HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _))) = do
      case lookupRetryAfter response of
        Just (RetryAfterSeconds seconds) -> return $ Stamina.RetryDelay $ secondsToNominalDiffTime $ fromIntegral seconds
        Just (RetryAfterDate date) -> return $ Stamina.RetryTime date
        Nothing ->
          if shouldRetryHttpException exc
            then return Stamina.Retry
            else return Stamina.RaiseException
    httpExceptionToRetryAction (Just exc) | shouldRetryHttpException exc = return Stamina.Retry
    httpExceptionToRetryAction _ = return Stamina.RaiseException

    -- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Retry-After
    lookupRetryAfter :: HTTP.Response body -> Maybe RetryAfterHeader
    lookupRetryAfter = readMaybe . show . snd . head . filter ((== hRetryAfter) . fst) . HTTP.responseHeaders

data RetryAfterHeader
  = RetryAfterDate UTCTime
  | RetryAfterSeconds Int
  deriving (Eq, Show)

instance Read RetryAfterHeader where
  readPrec = parseSeconds <|> parseWebDate
    where
      parseSeconds = RetryAfterSeconds <$> readPrec
      parseWebDate = ReadPrec.lift $ RetryAfterDate <$> readPTime True defaultTimeLocale rfc822DateFormat

shouldRetryHttpException :: HTTP.HttpException -> Bool
shouldRetryHttpException (HTTP.InvalidUrlException _ _) = False
shouldRetryHttpException (HTTP.HttpExceptionRequest _ reason) =
  case reason of
    HTTP.ConnectionClosed -> True
    HTTP.ConnectionFailure _ -> True
    HTTP.ConnectionTimeout -> True
    HTTP.IncompleteHeaders -> True
    HTTP.InternalException _ -> True
    HTTP.InvalidChunkHeaders -> True
    HTTP.InvalidProxyEnvironmentVariable _ _ -> True
    HTTP.InvalidStatusLine _ -> True
    HTTP.NoResponseDataReceived -> True
    HTTP.ProxyConnectException _ _ status
      | statusIsServerError status -> True
    HTTP.ResponseBodyTooShort _ _ -> True
    HTTP.ResponseTimeout -> True
    HTTP.StatusCodeException response _
      | HTTP.responseStatus response == tooManyRequests429 -> True
    HTTP.StatusCodeException response _
      | statusIsServerError (HTTP.responseStatus response) -> True
    HTTP.HttpZlibException _ -> True
    _ -> False
