{-# LANGUAGE OverloadedStrings #-}
import           Wheb

main :: IO ()
main = do
  opts <- genMinOpts $ addGET "." rootPat $ (text "Hi!")
  runWhebServer opts