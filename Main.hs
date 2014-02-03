{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class


import System.IO

import System.Exit
import Control.Concurrent

import Network.Wai
import qualified Data.Text.Lazy as T
import           Data.Maybe (fromJust)
import           Data.Monoid
import Web.Crunch
import Web.Crunch.Types
import Web.Crunch.Routes
import Web.Crunch.Plugins.Auth
import Web.Crunch.Plugins.Session

import Web.Crunch.Plugins.Debug.MemoryBackend

data GlobalApp = GlobalApp { sessContainer :: SessionContainer
                           , authContainer :: AuthContainer }

instance SessionApp GlobalApp where
  getSessionContainer = sessContainer

instance AuthApp GlobalApp where
  getAuthContainer = authContainer


homePage :: CrunchT GlobalApp String IO HandlerResponse
homePage = do
    v <- getSessionValue "has-visted"
    setSessionValue "has-visted" "True"
    case v of
      Just _  -> html "<h1>Welcome back!</h1>"
      Nothing -> html "<h1>Hello Stranger!</h1>"

handleSimple :: T.Text -> CrunchT GlobalApp String IO HandlerResponse
handleSimple t = html $ "<h1>" <> t <> "</h1>"

handlePOST :: CrunchT GlobalApp String IO HandlerResponse
handlePOST = do
    params <- getPOSTParams
    let keys = (fmap fst params)
        values = (fmap snd params)
    currentVals <- mapM getSessionValue keys
    let curValsText = zipWith (\k v -> "| Key: " <> k <> " Value: " <> (T.pack $ show v)) keys currentVals
    forM_ params (\(k, v) -> setSessionValue k v)
    html $ "<h1>Session Values before SET...</h1>" <>  (mconcat $ curValsText)

main :: IO ()
main = do
  opts <- generateOptions $ do
      -- | Add your application routes...
      addGET rootPat homePage
      addGET "faq" $ handleSimple "FAQ"
      -- | Overloaded URLs
      addPOST ("blog" </> (grabInt "pk")) handlePOST
      addGET  ("blog" </> (grabText "slug")) $ 
            (getRouteParam "slug") >>= (handleSimple . fromJust)
      
      -- | Initialize any backends.
      sess <- initSessionMemory
      auth <- initAuthMemory
      
      -- | Return your new global context.
      return (GlobalApp sess auth)
      
  forkIO $ runCrunchServer opts
  putStrLn "[ Press Enter for shutdown ]"
  exitOnInput

exitOnInput = do
  hSetBuffering stdin NoBuffering
  _ <- getChar
  exitSuccess