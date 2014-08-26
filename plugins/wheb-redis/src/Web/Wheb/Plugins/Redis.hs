module Web.Wheb.Plugins.Redis (
      runRedis
    , initRedis
    , RedisApp (..)
    , RedisContainer
    , module Database.Redis
    ) where

import Database.Redis hiding (runRedis)
import qualified Database.Redis as R
import Web.Wheb

data RedisContainer = RedisContainer Connection

class RedisApp a where
  getRedisContainer :: a -> RedisContainer

runWithContainer :: MonadIO m => RedisContainer -> Redis a -> WhebT g s m a
runWithContainer (RedisContainer con) r = liftIO $ R.runRedis con r

runRedis :: (RedisApp g, MonadIO m) => Redis a -> WhebT g s m a
runRedis r =  getWithApp getRedisContainer >>= flip runWithContainer r

initRedis :: MonadIO m => ConnectInfo -> InitM g s m RedisContainer
initRedis info = do
  conn <- liftIO $ connect info
  return $ RedisContainer conn
