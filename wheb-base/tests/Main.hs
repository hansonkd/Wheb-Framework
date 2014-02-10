module Main where

import Test.QuickCheck
import TestRoutes (checkURL)
import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "Running route tests:"
    quickCheck checkURL
