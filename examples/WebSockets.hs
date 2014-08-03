{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.ByteString.Lazy as B
import           Web.Wheb
import           Network.WebSockets as W

data MyApp = MyApp (TChan B.ByteString)
data MyHandlerData = MyHandlerData (TChan B.ByteString)

-- | This duplicates the connection for new Users
tchanMw :: MonadIO m => WhebMiddleware MyApp MyHandlerData m
tchanMw = do
  (MyApp chan) <- getApp
  newChan <- liftIO $ atomically $ dupTChan chan
  putHandlerState (MyHandlerData newChan)
  return Nothing

readHandler :: W.Connection -> WhebT MyApp MyHandlerData IO ()
readHandler c = do
    (MyHandlerData chan) <- getHandlerState
    forever $ liftIO $ do
        msg <- atomically $ readTChan chan
        W.sendTextData c msg

writeHandler :: W.Connection -> WhebT MyApp MyHandlerData IO ()
writeHandler c = do
    (MyHandlerData chan) <- getHandlerState
    forever $ liftIO $ do
        msg <- W.receiveDataMessage c
        let bmsg = case msg of
              W.Text m -> m
              W.Binary m -> m
        atomically $ writeTChan chan bmsg

main :: IO ()
main = do
  opts <- generateOptions $ do
            addWhebMiddleware tchanMw
            startingChan <- liftIO $ newTChanIO

            addWhebSocket (rootPat </> "read") readHandler
            addWhebSocket (rootPat </> "write") writeHandler

            return $ (MyApp startingChan, MyHandlerData startingChan)

  runWhebServer opts