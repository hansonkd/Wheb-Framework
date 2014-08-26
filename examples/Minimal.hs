{-# LANGUAGE OverloadedStrings #-}
import           Web.Wheb

main :: IO ()
main = do
  opts <- genMinOpts $ addGET "." rootPat $ (text "Hi!")
  runWhebServer opts