# Stamina.hs

**WIP: doesn't work yet, in a design phase.**

[![Hackage](https://img.shields.io/hackage/v/stamina.svg?style=flat)](https://hackage.haskell.org/package/stamina) ![CI status](https://github.com/cachix/stamina.hs/actions/workflows/ci.yml/badge.svg)

A retry Haskell library for humans:

- **Exponential backoff** with **jitter** between retries.
- Limit the **attempts** of retries and **total** time.
- `Stamina.HTTP` for retrying retriable `Network.HTTP.Client` exceptions.
- Introspectable retry state for logging using `RetryStatus`.

## API

```haskell

defaultRetrySettings :: RetrySettings

data RetryStatus = RetryStatus
  { attempts :: Int,
    delay :: Int,
    totalDelay :: Int
  }

-- Retry on all sync exceptions
retry :: MonadIO m => RetrySettings -> (RetryStatus -> m a) -> m a

-- Retry on specific exceptions
retryOnExceptions :: (Exception e, MonadIO m) => RetrySettings -> [Handler RetryAction] -> (RetryStatus -> m a) -> m a

data RetryAction = Skip | Retry | RetryAfter Int
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
- [retry](https://github.com/Soostone/retry) as case study for what needs to be supported