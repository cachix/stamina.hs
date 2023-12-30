module Main (main) where

import Control.Exception (fromException, try)
import Control.Monad.Catch (throwM)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (secondsToNominalDiffTime)
import Network.HTTP.Client qualified as HTTP
import Stamina qualified
import Stamina.HTTP qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Stamina" $ do
    it "should be able to retry until it recovers" $ do
      lastStatus <- newIORef $ Stamina.initialRetryStatus Stamina.defaults
      result <- Stamina.retry Stamina.defaults $ \status -> do
        if Stamina.attempts status < 5
          then throwM $ userError "error"
          else do
            writeIORef lastStatus status
            return "ok"
      result `shouldBe` "ok"
      status <- readIORef lastStatus
      Stamina.attempts status `shouldBe` 5
      Stamina.totalDelay status `shouldSatisfy` (> secondsToNominalDiffTime 8)
    it "should be able to retry until maxAttemps" $ do
      lastStatus <- newIORef $ Stamina.initialRetryStatus Stamina.defaults
      result <- try $ Stamina.retry (Stamina.defaults {Stamina.maxAttempts = Just 2}) $ \status -> do
        writeIORef lastStatus status
        throwM $ userError "error"
      (result :: Either IOError String) `shouldBe` Left (userError "error")
      status <- readIORef lastStatus
      Stamina.attempts status `shouldBe` 2
      Stamina.totalDelay status `shouldSatisfy` (< secondsToNominalDiffTime 5)
    it "should be able to retry until maxTime" $ do
      lastStatus <- newIORef $ Stamina.initialRetryStatus Stamina.defaults
      result <- try $ Stamina.retry (Stamina.defaults {Stamina.maxTime = Just $ secondsToNominalDiffTime 2}) $ \status -> do
        writeIORef lastStatus status
        throwM $ userError "error"
      (result :: Either IOError String) `shouldBe` Left (userError "error")
      status <- readIORef lastStatus
      Stamina.attempts status `shouldSatisfy` (< 3)
      Stamina.attempts status `shouldSatisfy` (> 0)
      Stamina.totalDelay status `shouldSatisfy` (< secondsToNominalDiffTime 3)
    it "should respect resetInitial the retry status" $ do
      lastStatus <- newIORef $ Stamina.initialRetryStatus Stamina.defaults
      result <- Stamina.retry Stamina.defaults $ \status -> do
        if Stamina.attempts status < 3
          && (Stamina.lastException status >>= fromException) /= Just (userError "error2")
          then throwM $ userError "error1"
          else
            if (Stamina.lastException status >>= fromException) == Just (userError "error1")
              then do
                Stamina.resetInitial status
                throwM $ userError "error2"
              else do
                writeIORef lastStatus status
                return "ok"
      result `shouldBe` "ok"
      status <- readIORef lastStatus
      Stamina.attempts status `shouldBe` 1
      Stamina.totalDelay status `shouldSatisfy` (< secondsToNominalDiffTime 2)

  describe "Stamina.HTTP" $ do
    it "should be able to retry on http exceptions until IO exception" $ do
      lastStatus <- newIORef $ Stamina.initialRetryStatus Stamina.defaults
      result <- try $ Stamina.HTTP.retry Stamina.defaults $ \status -> do
        writeIORef lastStatus status
        if Stamina.attempts status < 3
          then throwM $ HTTP.HttpExceptionRequest HTTP.defaultRequest HTTP.ResponseTimeout
          else throwM $ userError "error"
      (result :: Either IOError String) `shouldBe` Left (userError "error")
      status <- readIORef lastStatus
      Stamina.attempts status `shouldBe` 3

-- TODO: test RetryAfter
