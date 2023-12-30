module Stamina
  ( -- functions
    retry,
    retryOnExceptions,
    -- types
    RetrySettings (..),
    defaults,
    RetryAction (..),
    RetryStatus (..),
    -- raising exceptions
    escalateWith,
    escalate,
    withLeft,
  )
where

import Control.Concurrent (newMVar, putMVar)
import Control.Exception (Exception (..), SomeAsyncException (SomeAsyncException), SomeException, throwIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, throwM, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (isJust)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import System.Random (randomRIO)

-- | Settings for the retry functions.
data RetrySettings = RetrySettings
  { initialRetryStatus :: RetryStatus, -- Initial status of the retry, useful to override when resuming a retry
    maxAttempts :: Maybe Int, -- Maximum number of attempts. Can be combined with a timeout. Default to 10.
    maxTime :: Maybe DiffTime, -- Maximum time for all retries. Can be combined with attempts. Default to 60s.
    backoffMaxRetryDelay :: DiffTime, -- Maximum backoff between retries at any time. Default to 5s.
    backoffJitter :: Double, -- Maximum jitter that is added to retry back-off delays (the actual jitter added is a random number between 0 and backoffJitter). Defaults to 1.0.
    backoffExpBase :: Double -- The exponential base used to compute the retry backoff. Defaults to 2.0.
  }

-- | Tracks the status of a retry
--
-- All fields will be zero if no retries have been attempted yet.
data RetryStatus = RetryStatus
  { attempts :: Int, -- Number of retry attempts so far.
    delay :: DiffTime, -- Delay before the next retry.
    totalDelay :: DiffTime, -- Total delay so far.
    reset :: IO (), -- Reset the retry status to the initial state.
    lastException :: Maybe SomeException -- The last exception that was thrown.
  }

defaults :: IO RetrySettings
defaults = do
  resetMVar <- newMVar ()
  return $
    RetrySettings
      { initialRetryStatus =
          RetryStatus
            { attempts = 0,
              delay = 0,
              totalDelay = 0,
              reset = void $ putMVar resetMVar (),
              lastException = Nothing
            },
        maxAttempts = Just 10,
        maxTime = Just $ secondsToDiffTime 60,
        backoffMaxRetryDelay = 5.0,
        backoffJitter = 1.0,
        backoffExpBase = 2.0
      }

data RetryAction
  = RaiseException -- Propagate the exception.
  | Retry -- Retry with the delay according to the settings.
  | RetryDelay DiffTime -- Retry after the given delay.

-- | Retry on all sync exceptions, async exceptions will still be thrown.
--
-- The backoff delays between retries grow exponentially plus a random jitter.
-- The backoff for retry attempt number _attempt_ is computed as:
--
--    backoffInitial * backoffExpBase ** (attempt - 1) + random(0, backoffJitter)
--
-- Since x**0 is always 1, the first backoff is within the interval [backoff_initial,backoff_initial+backoff_jitter]. Thus, with default values between 0.1 and 1.1 seconds.

-- If all retries fail, the last exception is let through.
retry :: (MonadCatch m, MonadIO m) => RetrySettings -> (RetryStatus -> m a) -> m a
retry settings = retryOnExceptions settings skipAsyncExceptions
  where
    skipAsyncExceptions :: SomeException -> RetryAction
    skipAsyncExceptions exc = case fromException exc of
      Just (SomeAsyncException _) -> RaiseException
      Nothing -> Retry

-- TODO: implement reset
-- Same as retry, but only retry the given exceptions.
retryOnExceptions ::
  (Exception exc, MonadIO m, MonadCatch m) =>
  RetrySettings ->
  (exc -> RetryAction) ->
  (RetryStatus -> m a) ->
  m a
retryOnExceptions settings handler action =
  go $ initialRetryStatus settings
  where
    -- go :: (MonadCatch m, MonadIO m) => RetryStatus -> m a
    go retryStatus = do
      result <- try $ action retryStatus
      case result of
        Right out -> return out
        Left exception -> case handler exception of
          RaiseException -> throwM exception
          Retry -> do
            delay_ <- liftIO $ increaseDelay retryStatus
            maybeAttempt exception retryStatus delay_
          RetryDelay delay_ -> do
            maybeAttempt exception retryStatus delay_

    updateRetryStatus :: RetryStatus -> DiffTime -> SomeException -> RetryStatus
    updateRetryStatus status delay_ exception =
      status
        { attempts = attempts status + 1,
          delay = delay_,
          totalDelay = totalDelay status + delay_,
          lastException = Just exception
        }

    increaseDelay :: MonadIO m => RetryStatus -> m DiffTime
    increaseDelay retryStatus = do
      let RetryStatus {attempts} = retryStatus
      let RetrySettings {backoffMaxRetryDelay, backoffJitter, backoffExpBase} = settings
      jitter <- randomRIO (0, backoffJitter)
      return $ min backoffMaxRetryDelay $ secondsToDiffTime $ floor $ backoffExpBase ** (fromIntegral attempts - 1) + jitter

    -- maybeAttempt :: (Exception exc, MonadCatch m, MonadIO m) => exc -> RetryStatus -> DiffTime -> m a
    maybeAttempt exception retryStatus delay_ = do
      let RetrySettings {maxTime, maxAttempts} = settings
      if (isJust maxTime && Just (totalDelay retryStatus + delay_) > maxTime)
        || (isJust maxAttempts && Just (attempts retryStatus + 1) > maxAttempts)
        then throwM exception
        else go $ updateRetryStatus retryStatus delay_ $ toException exception

-- | Escalate an Either to an exception by converting the Left value to an exception.
escalateWith :: (Exception exc) => (err -> exc) -> Either err a -> IO a
escalateWith f = either (throwIO . f) return

-- | Convert a Maybe to an Either.
withLeft :: a -> Maybe b -> Either a b
withLeft a = maybe (Left a) Right

-- | Escalate an Either to an exception.
escalate :: (Exception exc) => Either exc a -> IO a
escalate = escalateWith id
