# Stamina

[![Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#concept) [![Hackage](https://img.shields.io/hackage/v/stamina.svg?style=flat)](https://hackage.haskell.org/package/stamina) ![CI status](https://github.com/cachix/stamina.hs/actions/workflows/ci.yml/badge.svg)

A retry Haskell library for humans:

- **Exponential backoff** with **jitter** between retries.
- Limit the **attempts** of retries and **total** time.
- **Stamina.HTTP** for retrying retriable [Network.HTTP.Client](https://hackage.haskell.org/package/http-client) exceptions respecting `Retry-After` headers.
- Introspectable retry state for logging using **RetryStatus**, including the last exception that occurred.
- Support **resetting the retry state** when for **long-running tasks** with an attempt that works.

## API

## Basics

- `Stamina.defaults :: (MonadIO m) => m RetrySettings`
- `Stamina.RetryStatus = RetryStatus { attempts :: Int, delay :: NominalDiffTime, totalDelay :: NominalDiffTime, resetInitial :: IO (), lastException :: Maybe SomeException }`
- `Stamina.retry :: (MonadCatch m, MonadIO m) => RetrySettings -> (RetryStatus -> m a) -> m a`

## Exceptions

- `Stamina.RetryAction = RaiseException | Retry | RetryDelay NominalDiffTime | RetryTime UTCTime`
- `Stamina.retryFor :: (MonadCatch m, MonadIO m, Exception exc) => RetrySettings -> (exc -> m RetryAction) -> (RetryStatus -> m a) -> m a`

## HTTP

- `Stamina.HTPP.retry :: (MonadIO m, MonadCatch m) => Stamina.RetrySettings -> (Stamina.RetryStatus -> m a) -> m a`

## Basic example

```haskell
import qualified Stamina
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO)

go :: IO ()
go = Stamina.retry Stamina.defaults $ \retryStatus -> do
  throwM $ userError "nope"
```

## Example to catch specific exceptions

```haskell

handler :: (MonadIO m) => IOError -> m Stamina.RetryAction
handler _ = return Stamina.Retry

go2 :: IO ()
go2 = Stamina.retryFor Stamina.defaults handler $ \retryStatus -> do
  throwM $ userError "nope"
```

## Development

1. Install [devenv.sh](https://devenv.sh/getting-started/).

2. `devenv shell`

3. `stack build`

## Credits

- Heavily inspired by [stamina for Python](https://stamina.hynek.me/en/stable/tutorial.html#retries).
- [retry](https://github.com/Soostone/retry) as case study for what needs to be supported.

<details>
  <summary>Test setup</summary>
  
  ```haskell
  main = undefined
  ```
</details>
