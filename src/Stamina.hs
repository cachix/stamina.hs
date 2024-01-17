{-# LANGUAGE ScopedTypeVariables #-}

module Stamina
  ( -- functions
    retry,
    retryFor,
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

import Control.Concurrent (MVar, isEmptyMVar, newEmptyMVar, threadDelay, tryPutMVar)
import Control.Exception (Exception (..), SomeAsyncException (SomeAsyncException), SomeException, throwIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, throwM, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (isJust)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import System.Random (randomRIO)

-- | Settings for the retry functions.
data RetrySettings = RetrySettings
  { -- | Initial status of the retry, useful to override when resuming a retry
    initialRetryStatus :: RetryStatus,
    -- | Maximum number of attempts. Can be combined with a timeout. Default to 10.
    maxAttempts :: Maybe Int,
    -- | Maximum time for all retries. Can be combined with attempts. Default to 60s.
    maxTime :: Maybe NominalDiffTime,
    -- | Maximum backoff between retries at any time. Default to 60s.
    backoffMaxRetryDelay :: Maybe NominalDiffTime,
    -- | Maximum jitter that is added to retry back-off delays (the actual jitter added is a random number between 0 and backoffJitter). Defaults to 1.0.
    backoffJitter :: Double,
    -- | The exponential base used to compute the retry backoff. Defaults to 2.0.
    backoffExpBase :: Double
  }

-- | Tracks the status of a retry
--
-- All fields will be zero if no retries have been attempted yet.
data RetryStatus = RetryStatus
  { -- | Number of retry attempts so far.
    attempts :: Int,
    -- | Delay before the next retry.
    delay :: NominalDiffTime,
    -- | Total delay so far.
    totalDelay :: NominalDiffTime,
    -- | Reset the retry status to the initial state.
    resetInitial :: IO (),
    -- | The last exception that was thrown.
    lastException :: Maybe SomeException
  }

defaults :: RetrySettings
defaults =
  RetrySettings
    { initialRetryStatus =
        RetryStatus
          { attempts = 0,
            delay = 0,
            totalDelay = 0,
            resetInitial = return (),
            lastException = Nothing
          },
      maxAttempts = Just 10,
      maxTime = Just $ secondsToNominalDiffTime 60,
      backoffMaxRetryDelay = Just $ secondsToNominalDiffTime 60.0,
      backoffJitter = 1.0,
      backoffExpBase = 2.0
    }

data RetryAction
  = RaiseException -- Propagate the exception.
  | Retry -- Retry with the delay according to the settings.
  | RetryDelay NominalDiffTime -- Retry after the given delay.
  | RetryTime UTCTime -- Retry after the given time.
  deriving (Show, Eq)

-- | Retry on all sync exceptions, async exceptions will still be thrown.
--
-- The backoff delays between retries grow exponentially plus a random jitter.
-- The backoff for retry attempt number _attempt_ is computed as:
--
-- @
--    backoffExpBase ** (attempt - 1) + random(0, backoffJitter)
-- @
--
-- With the default values, the backoff for the first 5 attempts will be:
--
-- @
--    2 ** 0 + random(0, 1) = 1 + random(0, 1)
--    2 ** 1 + random(0, 1) = 2 + random(0, 1)
--    2 ** 2 + random(0, 1) = 4 + random(0, 1)
--    2 ** 3 + random(0, 1) = 8 + random(0, 1)
--    2 ** 4 + random(0, 1) = 16 + random(0, 1)
-- @
--
-- If all retries fail, the last exception is let through.
retry :: forall m a. (MonadCatch m, MonadIO m) => RetrySettings -> (RetryStatus -> m a) -> m a
retry settings = retryFor settings skipAsyncExceptions
  where
    skipAsyncExceptions :: SomeException -> m RetryAction
    skipAsyncExceptions exc = case fromException exc of
      Just (SomeAsyncException _) -> return RaiseException
      Nothing -> return Retry

-- Same as retry, but only retry the given exceptions.
retryFor ::
  forall m exc a.
  (Exception exc, MonadIO m, MonadCatch m) =>
  RetrySettings ->
  (exc -> m RetryAction) ->
  (RetryStatus -> m a) ->
  m a
retryFor settings handler action = initialize >>= go
  where
    initialize = do
      resetMVar <- liftIO $ newEmptyMVar
      let retryStatus = (initialRetryStatus settings) {resetInitial = void $ tryPutMVar resetMVar ()}
      return (retryStatus, resetMVar)
    go :: (MonadCatch m, MonadIO m) => (RetryStatus, MVar ()) -> m a
    go (retryStatus, currentResetMVar) = do
      result <- try $ action retryStatus
      case result of
        Right out -> return out
        Left exception -> do
          (newRetryStatus, newResetMVar) <- do
            isEmpty <- liftIO $ isEmptyMVar currentResetMVar
            if isEmpty
              then return (retryStatus, currentResetMVar)
              else initialize
          exceptionAction <- handler exception
          delay_ <- case exceptionAction of
            RaiseException -> throwM exception
            Retry -> increaseDelay newRetryStatus
            RetryDelay delay_ -> return delay_
            RetryTime time -> liftIO $ diffUTCTime time <$> getCurrentTime
          let RetrySettings {maxTime, maxAttempts} = settings
          if (isJust maxTime && Just (totalDelay newRetryStatus + delay_) > maxTime)
            || (isJust maxAttempts && Just (attempts newRetryStatus) == maxAttempts)
            then throwM exception
            else do
              liftIO $ threadDelay $ round $ 1000 * 1000 * (nominalDiffTimeToSeconds delay_)
              go (updateRetryStatus newRetryStatus delay_ $ toException exception, newResetMVar)

    updateRetryStatus :: RetryStatus -> NominalDiffTime -> SomeException -> RetryStatus
    updateRetryStatus status delay_ exception =
      status
        { attempts = attempts status + 1,
          delay = delay_,
          totalDelay = totalDelay status + delay_,
          lastException = Just exception
        }

    increaseDelay :: (MonadIO m) => RetryStatus -> m NominalDiffTime
    increaseDelay retryStatus = do
      let RetryStatus {attempts} = retryStatus
      let RetrySettings {backoffMaxRetryDelay, backoffJitter, backoffExpBase} = settings
      jitter <- randomRIO (0, backoffJitter)
      let delay = secondsToNominalDiffTime $ realToFrac $ backoffExpBase ** (fromIntegral attempts - 1) + jitter
      return $ maybe delay (min delay) backoffMaxRetryDelay

-- | Escalate an Either to an exception by converting the Left value to an exception.
escalateWith :: (Exception exc) => (err -> exc) -> Either err a -> IO a
escalateWith f = either (throwIO . f) return

-- | Convert a Maybe to an Either.
withLeft :: a -> Maybe b -> Either a b
withLeft a = maybe (Left a) Right

-- | Escalate an Either to an exception.
escalate :: (Exception exc) => Either exc a -> IO a
escalate = escalateWith id
