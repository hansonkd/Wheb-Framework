-- | Shows how to use Wheb and CSRF together with Strapped.

{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

import           Control.Monad
import           Control.Monad.Except

import           Wheb
import           Wheb.Plugins.Strapped
import           Wheb.Plugins.Security
import           Text.Strapped

type MyApp = WhebT MyGlobalCtx () IO

data MyGlobalCtx = MyGlobalCtx (StrappedContainer MyApp)

instance StrappedApp MyGlobalCtx MyApp where
    getStrappedContainer (MyGlobalCtx g) = g

main :: IO ()
main = do
  opts <- generateOptions $ do
    sc <- initStrapped defaultConfig "examples/resources" ".html"

    addWhebMiddleware $ csrfMiddleware $ renderTemplate "csrf.html" emptyBucket

    addGET "." rootPat $ renderTemplate "form.html" emptyBucket
    addPOST "." rootPat $ text "You passed CSRF!"

    return (MyGlobalCtx sc, ())
  runWhebServer opts