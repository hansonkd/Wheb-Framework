{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text.Lazy (Text)
import           Wheb

data MyApp = MyApp Text (TVar Int)
data MyHandlerData = MyHandlerData Int

counterMw :: MonadIO m => WhebMiddleware MyApp MyHandlerData m
counterMw = do
  (MyApp _ ctr) <- getApp
  number <- liftIO $ atomically $ do
              num <- readTVar ctr
              writeTVar ctr (succ num)
              return num
  putHandlerState (MyHandlerData number)
  return Nothing

homePage :: WhebHandler MyApp MyHandlerData
homePage = do
  (MyApp appName _)   <- getApp
  (MyHandlerData num) <- getHandlerState
  html $ ("<h1>Welcome to" <> appName <> 
          "</h1><h2>You are visitor #" <> (spack num) <> "</h2>")

main :: IO ()
main = do
  opts <- generateOptions $ do
            startingCounter <- liftIO $ newTVarIO 0
            addWhebMiddleware counterMw
            addGET "." rootPat $ homePage
            return $ (MyApp "AwesomeApp" startingCounter, MyHandlerData 0)
  runWhebServer opts