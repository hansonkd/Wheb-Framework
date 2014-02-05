{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
 
main :: IO ()
main = do
  
  htmlContent <- LBS.readFile "examples/resources/index.html"
  run 8080 $ const $ return $ responseLBS
    status200
    [("Content-Type", "application/json")]
    htmlContent