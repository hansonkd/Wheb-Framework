{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_, forM)

import           Data.Maybe (fromJust, isJust)
import           Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)
import           Network.HTTP.Types.URI

import           Wheb
import           Wheb.WhebT (getRawRoute)
import           Wheb.Routes (matchUrl, testUrlParser)
import           Wheb.Utils (lazyTextToSBS)

myHandler :: L.Text -> MinHandler
myHandler = html

main :: IO ()
main = do
  opts <- genMinOpts $ do
    
    -- rootPat is the same as \"\"
    addGET "home" rootPat $ myHandler "Homepage"
    addGET "simple" "simple" $ myHandler "Simple"
    addGET "int" (rootPat </> (grabInt "pk")) $ myHandler "Int base"
    addGET "txt" ("" </> (grabText "slug")) $ myHandler "Text base"
    -- Will with or without trailing slash.
    addGET "blog_int" ("blog" </> (grabInt "pk")) $ myHandler "Blog Int"
    -- Will only match on trailing slash.
    addGET "blog_txt" ("blog" </> (grabText "slug") </> "") $
                                    (getRouteParam "slug") >>= (myHandler)
    -- Some long URL
    addGET "long" ("pages" </> "docs" </> "blah" </> "do" </> "dah" </> "h") $ 
                                                    myHandler "Long url"
    -- Properly escapses
    addGET "encode" ("encode" </> "! * )(") $ myHandler "Encode"
    -- Match regardless of type.
    catchAll $ myHandler "Anything"

  runRawHandler opts $ do
    liftIO $ putStrLn "Testing routes..."
    forM_ routeList (\(rn, p) -> (liftIO . print) =<< getRoute rn p)
    rs <- forM routeList (\(rn, p) -> do
                                     (Just r) <- getRawRoute rn p
                                     return $ testUrlParser (routeParser r) p)
    liftIO $ print rs

  runWhebServer opts
  where routeList =   [ ("home", [])
                      , ("simple", [])
                      , ("int", [("pk", MkChunk (3 :: Int))])
                      , ("txt", [("slug", MkChunk ("hey" :: Text))])
                      , ("blog_int", [("pk", MkChunk (3 :: Int))])
                      , ("blog_txt", [("slug", MkChunk ("hey" :: Text))])
                      , ("blog_txt", [("slug", MkChunk ("*9(" :: Text))])
                      , ("long", [])
                      , ("encode", []) ]