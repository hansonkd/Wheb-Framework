{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Monoid ((<>))
import           Data.Text.Lazy as T
import           Web.Wheb
import           Web.Wheb.Plugins.Hastache

data MyApp = MyApp T.Text
data MyState = MyState
type MyHandler = WhebHandler MyApp MyState


handleHome :: MyHandler
handleHome = renderTemplate "index" tc
  where tc = TemplateContext 
                  ([("name", "jacob"), ("unread", "5")] :: [(T.Text, T.Text)])

main :: IO ()
main = do
  opts <- generateOptions $ do
            addGET "home" rootPat handleHome
            
            initHastache "examples/resources/templates"
            
            return (MyApp "Tutorial App", MyState)
  runWhebServer opts