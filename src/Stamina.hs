{- A retry library should be able to:

- Wait between your retries: this is called a backoff.
- Not retry simultaneously with all your clients, so you must introduce randomness into your backoff: a jitter.
- Not retry forever. Sometimes a remote service is down indefinitely, and you must deal with it.

Inspired by https://stamina.hynek.me/en/stable/api.html
-}
module Stamina (retry, retryOnException, retryOnOutput, RetrySettings(..), defaultRetrySettings, RetryStatus(..)) where

data RetrySettings = RetrySettings {
  initialRetryStatus :: RetryStatus, -- Initial status of the retry, useful to override when resuming a retry
  attempts :: Int, -- Maximum number of attempts. Can be combined with timeout. Default to 10.
  timeout :: Double, -- Maximum time for all retries. Can be combined with attempts. Default to 45.0.
  backoffInitialDelay :: Double, -- Minimum backoff in seconds before the first retry. Default to 0.1.
  backoffMaxDelay :: Double, -- Maximum backoff in seconds between retries at any time. Default to 5.0.
  backoffJitter :: Double, -- Maximum jitter that is added to retry back-off delays (the actual jitter added is a random number between 0 and backoffJitter). Default to 1.0.
  backoffExpBase :: Double -- The exponential base used to compute the retry backoff. Default to 2.0.
} deriving (Show)

-- Tracks the status of a retry
-- All fields will be zero if no retries have been attempted yet.
data RetryStatus = RetryStatus {
  attempts :: Int,
  delay :: Int,
  totalDelay :: Int,
} deriving (Show)

defaultRetrySettings :: RetrySettings
defaultRetrySettings = RetrySettings {
  initialRetryStatus=RetryStatus { attempts=0, delay=0, totalDelay=0 },
  attempts=10,
  timeout=45.0,
  backoffInitialDelay=0.1,
  backoffMaxDelay=5.0,
  backoffJitter=1.0,
  backoffExpBase=2.0
}

data RetryAction = Skip | Retry | RetryAfter Int

-- | Retry on all sync exceptions, async exceptions will still be thrown.
--
-- The backoff delays between retries grow exponentially plus a random jitter.
-- The backoff for retry attempt number _attempt_ is computed as:
--
--    backoffInitial * backoffExpBase ** (attempt - 1) + random(0, backoffJitter)
--
-- Since x**0 is always 1, the first backoff is within the interval [backoff_initial,backoff_initial+backoff_jitter]. Thus, with default values between 0.1 and 1.1 seconds.

-- If all retries fail, the last exception is let through.
retry :: MonadIO m => RetrySettings -> (RetryStatus -> m a) -> m a
retry settings action = undefined

-- Same as retry, but only retry on the given exceptions.
retryOnException :: (Exception e, MonadIO m) => RetrySettings -> (e -> m RetryAction) -> (RetryStatus -> m a) -> m a
retryOnException settings willRetry action = undefined

-- Same as retry, but only retry if the given predicate returns True on the output.
retryOnOutput :: MonadIO m => RetrySettings -> (RetryStatus -> a -> m RetryAction) -> (RetryStatus -> m a) -> m a
retryOnOutput settings willRetry action = undefined
