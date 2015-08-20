{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Wheb.Plugins.Redis (
      runRedis
    , initRedis
    , initRedisCache
    , RedisApp (..)
    , RedisCacheApp (..)
    , RedisContainer
    , RedisCacheContainer
    , module Database.Redis
    ) where
import Control.Monad (void, liftM)
import Data.Monoid ((<>))
import Data.ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Redis hiding (runRedis)
import qualified Database.Redis as R
import Wheb
import Wheb.Plugins.Cache
import Wheb.Plugins.Auth
import Wheb.Plugins.Session


sessionPrefix = T.pack "session"
userPrefix = T.pack "user"

-- | A container to use as a DB
data RedisContainer = RedisContainer Connection

-- | A seperate instance for cache
data RedisCacheContainer = RedisCacheContainer Connection

class RedisApp a where
  getRedisContainer :: a -> RedisContainer

instance RedisApp a => SessionApp a where
  getSessionContainer = SessionContainer . getRedisContainer

instance RedisApp a => AuthApp a where
  getAuthContainer = AuthContainer . getRedisContainer

class RedisCacheApp a where
  getRedisCacheContainer :: a -> RedisCacheContainer

instance RedisCacheApp a => CacheApp a where
  getCacheContainer = CacheContainer . getRedisCacheContainer
  
instance CacheBackend RedisCacheContainer where
  backendCachePut key content expr con = do
    mvoid $ runWithCacheContainer con $ setex (T.encodeUtf8 key) expr content
  backendCacheGet key con = do
      e <- runWithCacheContainer con $ get (T.encodeUtf8 key)
      return $ either (const Nothing) id e
  backendCacheDelete key con = do
      mvoid $ runWithCacheContainer con $ del [(T.encodeUtf8 key)]

instance SessionBackend RedisContainer where
  backendSessionPut sessId key content mc = do
    mvoid $ runWithContainer mc $ do
      hset (makeKey sessionPrefix sessId) (T.encodeUtf8 key) (T.encodeUtf8 content)
  backendSessionGet sessId key mc =  do
    runWithContainer mc $ do
      e <- hget (makeKey sessionPrefix sessId) (T.encodeUtf8 key)
      return $ either (const Nothing) (fmap T.decodeUtf8) e
  backendSessionDelete sessId key mc = do
    mvoid $ runWithContainer mc $
      hdel (makeKey sessionPrefix sessId) [T.encodeUtf8 key]
  backendSessionClear sessId mc = do
    mvoid $ runWithContainer mc $ do
      let sessk = (makeKey sessionPrefix sessId)
      ek <- hkeys sessk
      either (const $ return ()) (mvoid . hdel sessk) ek

instance AuthBackend RedisContainer where
  backendGetUser name mc = do
    runWithContainer mc $ do
      n <- get (makeKey userPrefix name)
      return $ either (const Nothing) (const $ Just $ AuthUser name) n
  backendLogin name pw mc =  do
    passCheck <- runWithContainer mc $ do
      n <- get (makeKey userPrefix name)
      return $ either (const Nothing) (fmap (verifyPw pw . T.decodeUtf8)) n
    case passCheck of
        Just True  -> return (Right $ AuthUser $ name)
        Just False -> return (Left InvalidPassword)
        Nothing    -> return (Left UserDoesNotExist)
  backendRegister user@(AuthUser name) pw mc =  do
    pwHash <- makePwHash pw
    runWithContainer mc $ do
      n <- get (makeKey userPrefix name)
      case n of
        Right Nothing -> do
          set (makeKey userPrefix name) (T.encodeUtf8 pwHash)
          return (Right $ user)
        _ -> return (Left DuplicateUsername)
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

makeKey :: T.Text -> T.Text -> ByteString
makeKey a b = T.encodeUtf8 $ a <> (T.pack ":") <> b

mvoid :: Monad m => m a -> m ()
mvoid a = a >> return ()

runWithCacheContainer :: MonadIO m => RedisCacheContainer -> Redis a -> WhebT g s m a
runWithCacheContainer (RedisCacheContainer con) r = liftIO $ R.runRedis con r

runWithContainer :: MonadIO m => RedisContainer -> Redis a -> WhebT g s m a
runWithContainer (RedisContainer con) r = liftIO $ R.runRedis con r

-- | Run a Redis command inside of WhebT
runRedis :: (RedisApp g, MonadIO m) => Redis a -> WhebT g s m a
runRedis r =  getWithApp getRedisContainer >>= flip runWithContainer r

-- | Initialize Redis.
initRedis :: MonadIO m => ConnectInfo -> InitM g s m RedisContainer
initRedis info = do
  conn <- liftIO $ connect info
  return $ RedisContainer conn

-- | Initialize a container for using redis as a cache. You will probably have a different
--  DB and settings for your data and cache so this is broken out.
initRedisCache :: MonadIO m => ConnectInfo -> InitM g s m RedisCacheContainer
initRedisCache info = do
  conn <- liftIO $ connect info
  return $ RedisCacheContainer conn