module Stamina.HTTP (retry) where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Client qualified as HTTP
import Stamina qualified

handler :: SomeException -> Stamina.RetryAction
handler = undefined

retry :: (MonadIO m, MonadCatch m) => Stamina.RetrySettings -> (Stamina.RetryStatus -> m a) -> m a
retry settings = Stamina.retryOnExceptions settings handler
