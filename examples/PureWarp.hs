{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
 
main :: IO ()
main = do
  run 8080 $ const $ return $ responseFile
    status200
    [("Content-Type", "application/json")]
    "examples/resources/index.html"
    Nothing