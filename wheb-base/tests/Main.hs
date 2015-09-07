{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Test.QuickCheck
import TestRoutes (checkURL)
import System.Exit (exitFailure)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad
import Data.Maybe (isNothing, isJust)
import Data.Either (isRight)

import qualified Data.Text.Lazy as T

import           Wheb
import           Wheb.Plugins.Debug.MemoryBackend
import           Wheb.Plugins.Auth
import           Wheb.Plugins.Session
import           Wheb.Plugins.Cache


data GlobalApp = GlobalApp { sessContainer :: SessionContainer
                           , authContainer :: AuthContainer
                           , cacheContainer :: CacheContainer }

data RequestState = RequestState { curUser :: Maybe AuthUser }

-- | Let our plugins know where their data is
instance SessionApp GlobalApp where
  getSessionContainer = sessContainer

instance AuthApp GlobalApp where
  getAuthContainer = authContainer

instance CacheApp GlobalApp where
  getCacheContainer = cacheContainer
  
-- | Needed for Auth middleware
instance AuthState RequestState where
  getAuthUser = curUser
  modifyAuthUser f c = c { curUser = f (curUser c) }
 

testCache opts = do
    void $ runRawHandler opts $  do
        deleteCacheValue "hello"
        
        v <- getCacheValue "hello"
        liftIO $ assertBool ("Expected Nothing") (isNothing v)
        
        setCacheValue "hello" "Hi"
        v <- getCacheValue "hello"
        liftIO $ assertEqual ("Expected Just Hi") v (Just "Hi")

testSessions opts = do
    void $ runRawHandler opts $ do
        
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
        -- | Initialize any backends.
        sess <- initSessionMemory
        auth <- initAuthMemory
        cache <- initCacheMemory
        
        -- | Return your new global context.
        return (GlobalApp sess auth cache, RequestState Nothing)     
    
    defaultMain $ testGroup "Mongo" [ testCase "testLogin" $ testLogin opts
                                    , testCase "testSession" $ testSessions opts
                                    , testCase "testCache" $ testCache opts
                                    , testProperty "testUrls" checkURL]

