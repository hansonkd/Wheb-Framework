{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UndecidableInstances #-}

module Wheb.Plugins.Groundhog (
      runRedis
    , initRedis
    , RedisApp (..)
    , RedisCacheApp (..)
    , RedisContainer
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
import Wheb.Plugins.Session


sessionPrefix = T.pack "session"
userPrefix = T.pack "user"

-- | A container to use as a DB
data RedisContainer = RedisContainer Connection

class RedisApp a where
  getRedisContainer :: a -> RedisContainer

instance RedisApp a => SessionApp a where
  type SessionAppBackend a = RedisContainer
  getSessionContainer = getRedisContainer


class RedisCacheApp a where
  getRedisCacheContainer :: a -> RedisContainer

instance RedisCacheApp a => CacheApp a where
  type CacheAppBackend a = RedisContainer
  getCacheContainer = getRedisCacheContainer
  
instance CacheBackend RedisContainer where
  backendCachePut key content expr con =
    mvoid $ runWithCacheContainer con $ setex (T.encodeUtf8 key) expr content
  backendCacheGet key con = do
      e <- runWithCacheContainer con $ get (T.encodeUtf8 key)
      return $ either (const Nothing) id e
  backendCacheDelete key con =
      mvoid $ runWithCacheContainer con $ del [T.encodeUtf8 key]

instance SessionBackend RedisContainer where
  backendSessionPut sessId key content mc =
    mvoid $ runWithContainer mc $
      hset (makeKey sessionPrefix sessId) (T.encodeUtf8 key) (T.encodeUtf8 content)
  backendSessionGet sessId key mc =
    runWithContainer mc $ do
      e <- hget (makeKey sessionPrefix sessId) (T.encodeUtf8 key)
      return $ either (const Nothing) (fmap T.decodeUtf8) e
  backendSessionDelete sessId key mc =
    mvoid $ runWithContainer mc $
      hdel (makeKey sessionPrefix sessId) [T.encodeUtf8 key]
  backendSessionClear sessId mc =
    mvoid $ runWithContainer mc $ do
      let sessk = makeKey sessionPrefix sessId
      ek <- hkeys sessk
      either (const $ return ()) (mvoid . hdel sessk) ek

makeKey :: T.Text -> T.Text -> ByteString
makeKey a b = T.encodeUtf8 $ a <> T.pack ":" <> b

mvoid :: Monad m => m a -> m ()
mvoid a = a >> return ()

runWithCacheContainer :: MonadIO m => RedisContainer -> Redis a -> WhebT g s m a
runWithCacheContainer (RedisContainer con) r = liftIO $ R.runRedis con r

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
