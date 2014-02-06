{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text.Lazy (Text, pack)
import           Web.Wheb

data MyApp = MyApp Text (TVar Int)
data MyHandlerData = MyHandlerData Int

instance Default MyHandlerData where
  def = MyHandlerData 0

counterMw :: MonadIO m => WhebMiddleware MyApp MyHandlerData m
counterMw = do
  (MyApp _ ctr) <- getApp
  number <- liftIO $ readTVarIO ctr
  liftIO $ atomically $ writeTVar ctr (succ number)
  putReqState (MyHandlerData number)
  return Nothing

homePage :: WhebHandler MyApp MyHandlerData
homePage = do
  (MyApp appName _)   <- getApp
  (MyHandlerData num) <- getReqState
  html $ ("<h1>Welcome to" <> appName <> 
          "</h1><h2>You are visitor #" <> (pack $ show num) <> "</h2>")

main :: IO ()
main = do
  opts <- generateOptions $ do
            startingCounter <- liftIO $ newTVarIO 0
            addWhebMiddleware counterMw
            addGET (pack ".") rootPat $ homePage
            return $ MyApp "AwesomeApp" startingCounter
  runWhebServer opts