{- A retry library should be able to:

- Wait between your retries: this is called a backoff.
- Not retry simultaneously with all your clients, so you must introduce randomness into your backoff: a jitter.
- Not retry forever. Sometimes a remote service is down indefinitely, and you must deal with it.

Inspired by https://stamina.hynek.me/en/stable/api.html
-}
module Stamina
  ( retry,
    retryOnExceptions,
    RetrySettings (..),
    defaults,
    RetryAction (..),
    RetryStatus (..),
  )
where

import Control.Exception (Exception, Handler)
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock (DiffTime)

data RetrySettings = RetrySettings
  { initialRetryStatus :: RetryStatus, -- Initial status of the retry, useful to override when resuming a retry
    maxAttempts :: Int, -- Maximum number of attempts. Can be combined with a timeout. Default to 10.
    maxTime :: DiffTime, -- Maximum time for all retries. Can be combined with attempts. Default to 45.0.
    backoffInitialRetryDelay :: DiffTime, -- Minimum backoff before the first retry. Default to 0.1.
    backoffMaxRetryDelay :: DiffTime, -- Maximum backoff between retries at any time. Default to 5.0.
    backoffJitter :: Double, -- Maximum jitter that is added to retry back-off delays (the actual jitter added is a random number between 0 and backoffJitter). Default to 1.0.
    backoffExpBase :: Double -- The exponential base used to compute the retry backoff. Defaults to 2.0.
  }
  deriving (Show)

-- Tracks the status of a retry
-- All fields will be zero if no retries have been attempted yet.
data RetryStatus = RetryStatus
  { attempts :: Int,
    delay :: DiffTime,
    totalDelay :: DiffTime
  }
  deriving (Show)

defaults :: RetrySettings
defaults =
  RetrySettings
    { initialRetryStatus = RetryStatus {attempts = 0, delay = 0, totalDelay = 0},
      maxAttempts = 10,
      maxTime = 45.0,
      backoffInitialRetryDelay = 0.1,
      backoffMaxRetryDelay = 5.0,
      backoffJitter = 1.0,
      backoffExpBase = 2.0
    }

data RetryAction = 
   Skip -- Propagate the exception.
 | Retry  -- Retry with the delay according to the settings.
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
retry :: MonadIO m => RetrySettings -> (RetryStatus -> m a) -> m a
retry settings action = undefined

-- Same as retry, but only retry on the given exceptions.
retryOnExceptions :: MonadIO m => RetrySettings -> [Handler RetryAction] -> (RetryStatus -> m a) -> m a
retryOnExceptions settings handlers action = undefined
