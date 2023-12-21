# Stamina

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept) [![Hackage](https://img.shields.io/hackage/v/stamina.svg?style=flat)](https://hackage.haskell.org/package/stamina) ![CI status](https://github.com/cachix/stamina.hs/actions/workflows/ci.yml/badge.svg)

A retry Haskell library for humans:

- **Exponential backoff** with **jitter** between retries.
- Limit the **attempts** of retries and **total** time.
- `Stamina.HTTP` for retrying retriable `Network.HTTP.Client` exceptions.
- Introspectable retry state for logging using `RetryStatus`.
- Support resetting the retry state when the action is long-running and an attempt works.

## API

```haskell
import Control.Exception (Exception, Handler)
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock (DiffTime)

defaults :: RetrySettings

data RetryStatus = RetryStatus
  { attempts :: Int,
    delay :: DiffTime,
    totalDelay :: DiffTime,
    reset :: IO ()
  }

-- Retry on all sync exceptions
retry :: MonadIO m 
      => RetrySettings 
      -> (RetryStatus -> m a)
      -> m a

-- Retry on specific exceptions
retryOnExceptions :: (Exception e, MonadIO m) 
                  => RetrySettings 
                  -> [Handler RetryAction] 
                  -> (RetryStatus -> m a)
                  -> m a

data RetryAction = 
   Skip -- Propagate the exception.
 | Retry  -- Retry with the delay according to the settings.
 | RetryDelay DiffTime -- Retry after the given delay.
```

## Example

```haskell

import qualified Stamina

main :: IO ()
main = do
    Stamina.retry Stamina.defaults $ \retryStatus -> do
        ... monadic logic that raises exceptions

```

## Development

1. Install [devenv.sh](https://devenv.sh/getting-started/).

2. `devenv shell`

3. `stack build`

## Credits

- Heavily inspired by [stamina for Python](https://stamina.hynek.me/en/stable/tutorial.html#retries).
- [retry](https://github.com/Soostone/retry) as case study for what needs to be supported.
