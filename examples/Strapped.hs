
{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

import           Control.Monad.Except

import           Web.Wheb
import           Web.Wheb.Plugins.Strapped
import           Text.Strapped

type MyApp = WhebT MyGlobalCtx () IO

data MyGlobalCtx = MyGlobalCtx (StrappedContainer MyApp)

instance StrappedApp MyGlobalCtx MyApp where
    getStrappedContainer (MyGlobalCtx g) = g

main :: IO ()
main = do
  opts <- generateOptions $ do
  	sc <- initStrapped "examples/resources" ".html"
  	addGET "." rootPat $ renderTemplate "index.html" (emptyBucket)
  	return (MyGlobalCtx sc, ())
  runWhebServer opts