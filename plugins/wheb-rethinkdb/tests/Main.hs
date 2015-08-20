{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Maybe (isNothing, isJust)
import Data.Either (isRight)

import qualified Data.Text.Lazy as T

import           Wheb
import           Wheb.Plugins.RethinkDB
import           Wheb.Plugins.Auth
import           Wheb.Plugins.Session

import qualified Database.RethinkDB.Network as R

data MyApp = MyApp RethinkContainer
data MyRequestState = MyRequestState { curUser :: Maybe AuthUser }

-- | Needed for Auth middleware
instance AuthState MyRequestState where
  getAuthUser = curUser
  modifyAuthUser f c = c { curUser = f (curUser c) }

instance RethinkApp MyApp where
    getRethinkContainer (MyApp mc) = mc

userTable = (table "users") { tablePrimaryKey = Just "username" }
sessTable = (table "sessions") { tablePrimaryKey = Just "sessId" }


testSessions opts = do
    void $ runRawHandler opts $ do
        runDb' $ tableDrop sessTable
        runDb' $ tableCreate sessTable
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool ("Expected Nothing") (isNothing k)
        
        setSessionValue "someKey" "woot"
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool ("Expected woot") ((Just "woot") == k)
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool ("Expected False") ((Just "____") /= k)
        
        deleteSessionValue "someKey"
        
        k <- getSessionValue "someKey"
        liftIO $ assertBool ("Expected Nothing") (isNothing k)

testLogin opts = do
    void $ runRawHandler opts $  do
        runDb' $ tableDrop userTable
        runDb' $ tableCreate userTable
        
        runDb' $ tableDrop sessTable
        runDb' $ tableCreate sessTable
                
        u <- getCurrentUser
        liftIO $ assertBool ("Expected Nothing") (isNothing u)
        
        loggedIn <- login "Joe" "123"
        liftIO $ assertBool ("Expected UserDoesNotExist") (ude loggedIn)
        
        reg1 <- register (AuthUser "Joe") "123"
        liftIO $ assertBool ("Expected User") (isRight reg1)
        
        loggedIn2 <- login "Joe" "abc"
        liftIO $ assertBool ("Expected InvalidPassword") (inv loggedIn2)
        
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
          
          inv (Left InvalidPassword) = True
          inv _ = False

main :: IO ()
main = do 
    opts <- generateOptions $ do
      rethink@(RethinkContainer h) <- initRethinkDB "127.0.0.1" 28015 Nothing Nothing
      liftIO $ print $ R.rdbDatabase $ h      
      return (MyApp rethink, MyRequestState Nothing)
    
    defaultMain $ testGroup "Mongo" [ testCase "testLogin" $ testLogin opts
                                    , testCase "testSession" $ testSessions opts]