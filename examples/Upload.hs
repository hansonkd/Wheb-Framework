{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import qualified Data.Text.Lazy as T

import           Network.Wai.Parse (fileName, fileContent)
import           Web.Wheb

uploadHandle :: MinHandler
uploadHandle = do
    (_, files) <- getRawPOST
    text $ T.intercalate ", " $ map (spack . fileName . snd) files

main :: IO ()
main = do
  opts <- genMinOpts $ do
    addPOST "upload" "upload" $ uploadHandle
  runWhebServer opts