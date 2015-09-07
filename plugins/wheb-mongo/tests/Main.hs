{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Maybe (isNothing, isJust)
import Data.Either (isRight)

import qualified Data.Text.Lazy as T

import           Wheb
import           Wheb.Plugins.Mongo
import           Wheb.Plugins.Session

data MyApp = MyApp MongoContainer
data MyRequestState = MyRequestState

instance MongoApp MyApp where
    getMongoContainer (MyApp mc) = mc

testSessions opts =
    void $ runRawHandler opts $ do
        sessCol <- getSessionCollection
        runAction $ delete (select [] sessCol)

        k <- getSessionValue "someKey"
        liftIO $ assertBool "Expected Nothing" (isNothing k)
        
        setSessionValue "someKey" "woot"
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool "Expected woot" ((Just "woot") == k)
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool "Expected False" ((Just "____") /= k)
        
        deleteSessionValue "someKey"
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool "Expected Nothing" (isNothing k)

testRaw opts = do
    Right results <- runRawHandler opts $ runAction $ do
        delete (select [] "test")
        insertMany "test" [
            ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
            ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
            ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
            ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]
        r <- rest =<< find (select [] "test") {sort = ["home.city" =: 1]}
        delete (select [] "test")
        return r
    assertEqual "Expected 4 documents" 4 (length results)

main :: IO ()
main = do 
    opts <- generateOptions $ do
      mongo <- initMongo "127.0.0.1:27017" "test"
      return (MyApp mongo, MyRequestState)
    
    defaultMain $ testGroup "Mongo" [ testCase "testRaw" $ testRaw opts
                                    , testCase "testSessions" $ testSessions opts]