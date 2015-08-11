{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Maybe (isNothing, isJust)
import Data.Either (isRight)

import qualified Data.Text.Lazy as T

import           Web.Wheb
import           Web.Wheb.Plugins.Mongo
import           Web.Wheb.Plugins.Auth

data MyApp = MyApp MongoContainer
data MyRequestState = MyRequestState { curUser :: Maybe AuthUser }

-- | Needed for Auth middleware
instance AuthState MyRequestState where
  getAuthUser = curUser
  modifyAuthUser f c = c { curUser = f (curUser c) }

instance MongoApp MyApp where
    getMongoContainer (MyApp mc) = mc

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
    assertEqual ("Expected 4 documents") 4 (length results)

testLogin opts = do
    void $ runRawHandler opts $  do
        runAction $ delete (select [] "users")
        
        u <- getCurrentUser
        liftIO $ assertBool ("Expected Nothing") (isNothing u)
        
        loggedIn <- login "Joe" "123"
        liftIO $ assertBool ("Expected UserDoesNotExist") (ude loggedIn)
        
        reg1 <- register (AuthUser "Joe") "123"
        liftIO $ assertBool ("Expected User") (isRight reg1)
        
        loggedIn2 <- login "Joe" "123"
        liftIO $ assertBool ("Expected login success") (isRight loggedIn2)
        
        reg2 <- register (AuthUser "Joe") "123"
        liftIO $ assertBool ("Expected DuplicateUsername error") (dup reg2)
        
        cur <- getCurrentUser
        liftIO $ assertBool ("Expected Just User") (isJust cur)
    
    where ude (Left UserDoesNotExist) = True
          ude _ = False
          
          dup (Left DuplicateUsername) = True
          dup _ = False

main :: IO ()
main = do 
    opts <- generateOptions $ do
      mongo <- initMongo "127.0.0.1:27017" "test"
      return (MyApp mongo, MyRequestState Nothing)
    
    defaultMain $ testGroup "Mongo" [ testCase "testRaw" $ testRaw opts
                                    , testCase "testLogin" $ testLogin opts]