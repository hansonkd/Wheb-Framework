{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Web.Wheb
import qualified Data.Text.Lazy as T

data MyApp = MyApp T.Text
data MyState = MyState
type MyHandler = WhebHandler MyApp MyState

handleHome :: MyHandler
handleHome = do
  url <- getRoute "echo" [("msg", MkChunk ("My Awesome message" :: T.Text))] 
  html $ "<html><body><a href=\"" <> url <> "\">Echo My Awesome message</a></body></html>!"
  
handleEcho :: MyHandler
handleEcho = do
  msg <- getRouteParam "msg"
  text $ "Msg was: " <> msg
  
main :: IO ()
main = do
  opts <- generateOptions $ do
            addGET "home" rootPat handleHome
            addGET "echo" ("echo" </> (grabText "msg")) handleEcho
            return (MyApp "Tutorial App", MyState)
  runWhebServer opts