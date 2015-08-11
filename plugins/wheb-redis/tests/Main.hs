{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Maybe (isNothing, isJust)
import Data.Either (isRight)
import System.Exit

import qualified Data.Text.Lazy as T

import           Web.Wheb
import           Web.Wheb.Plugins.Auth
import           Web.Wheb.Plugins.Redis
import           Web.Wheb.Plugins.Cache

data MyCtx = MyCtx RedisContainer RedisCacheContainer
data MyRequestState = MyRequestState { curUser :: Maybe AuthUser }

-- | Needed for Auth middleware
instance AuthState MyRequestState where
  getAuthUser = curUser
  modifyAuthUser f c = c { curUser = f (curUser c) }

instance RedisApp MyCtx where
  getRedisContainer (MyCtx rc _) = rc

instance RedisCacheApp MyCtx where
  getRedisCacheContainer (MyCtx _ rc) = rc

testLogin opts = do

    void $ runRawHandler opts $ do
        runRedis $ flushdb
        
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


testCache opts = do
    void $ runRawHandler opts $  do
        deleteCacheValue "hello"
        
        v <- getCacheValue "hello"
        liftIO $ assertBool ("Expected Nothing") (isNothing v)
        
        setCacheValue "hello" "Hi"
        v <- getCacheValue "hello"
        liftIO $ assertEqual ("Expected Just Hi") v (Just "Hi")



main :: IO ()
main = do 
    opts <- generateOptions $ do
        r <- initRedis defaultConnectInfo
        c <- initRedisCache defaultConnectInfo
        return (MyCtx r c, MyRequestState Nothing)
    
    defaultMain $ testGroup "Redis" [ testCase "testLogin" $ testLogin opts
                                    , testCase "testCache" $ testCache opts]
