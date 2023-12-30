# Stamina

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept) [![Hackage](https://img.shields.io/hackage/v/stamina.svg?style=flat)](https://hackage.haskell.org/package/stamina) ![CI status](https://github.com/cachix/stamina.hs/actions/workflows/ci.yml/badge.svg)

A retry Haskell library for humans:

- **Exponential backoff** with **jitter** between retries.
- Limit the **attempts** of retries and **total** time.
- `Stamina.HTTP` for retrying retriable `Network.HTTP.Client` exceptions.
- Introspectable retry state for logging using `RetryStatus`, including the exception that occured.
- Support resetting the retry state when the action is long-running and an attempt works.

## API

- `RetryAction`
- `RetryStatus`
- `defaults`
- `retry`
- `retryOnExceptions`

## Example

```haskell
import qualified Stamina
import Control.Monad.Catch (throwM)

go :: IO ()
go = do 
    defaults <- Stamina.defaults
    Stamina.retry defaults $ \retryStatus -> do
        throwM $ userError "nope"
```

## Example to catch specific exceptions

```haskell

isDoesNotExistError :: IOError -> Stamina.RetryAction
isDoesNotExistError _ = Stamina.Retry

go2 :: IO ()
go2 = do 
    defaults <- Stamina.defaults
    Stamina.retryOnExceptions defaults (return . isDoesNotExistError) $ \retryStatus -> do
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