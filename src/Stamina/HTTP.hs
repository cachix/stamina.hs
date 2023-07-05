module Stamina.HTTP (retry) where

import Control.Exception (Handler)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Client qualified as HTTP
import Stamina qualified

handlers :: [Handler Stamina.RetryAction]
handlers = undefined

retry :: MonadIO m => Stamina.RetrySettings -> (Stamina.RetryStatus -> m a) -> m a
retry settings action = Stamina.retryOnExceptions settings handlers action
