{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Web.Wheb.Plugins.Cache
  ( CacheContainer (..)
  , CacheApp (..)
  , CacheBackend (..)
  
  , setCacheValue
  , setCacheValue'
  , getCacheValue
  , getCacheValue'
  , deleteCacheValue
  ) where
    
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.ByteString as BS (ByteString)
import Web.Wheb

data CacheContainer = forall r. CacheBackend r => CacheContainer r

class CacheApp a where
  getCacheContainer :: a -> CacheContainer

class CacheBackend c where
  backendCachePut    :: (CacheApp a, MonadIO m) => Text -> ByteString -> Integer -> c -> WhebT a b m ()
  backendCacheGet    :: (CacheApp a, MonadIO m) => Text -> c -> WhebT a b m (Maybe ByteString)
  backendCacheDelete :: (CacheApp a, MonadIO m) => Text -> c -> WhebT a b m ()

runWithContainer :: (CacheApp a, MonadIO m) => (forall r. CacheBackend r => r -> WhebT a s m b) -> WhebT a s m b
runWithContainer f = do
  CacheContainer cacheStore <- getWithApp getCacheContainer
  f cacheStore

deleteCacheValue :: (CacheApp a, MonadIO m) => Text -> WhebT a b m ()
deleteCacheValue key = runWithContainer $ backendCacheDelete key

-- | Set a cache value with an expiration of an hour
setCacheValue :: (CacheApp a, MonadIO m) => Text -> ByteString -> WhebT a b m ()
setCacheValue key content = setCacheValue' key content (60 * 60)

-- | Set a cache value with expiration in seconds
setCacheValue' :: (CacheApp a, MonadIO m) => Text -> ByteString -> Integer -> WhebT a b m ()
setCacheValue' key content expr = runWithContainer $ backendCachePut key content expr

getCacheValue :: (CacheApp a, MonadIO m) => Text -> WhebT a b m (Maybe ByteString)
getCacheValue key = runWithContainer $ backendCacheGet key

getCacheValue' :: (CacheApp a, MonadIO m) => ByteString -> Text -> WhebT a b m ByteString
getCacheValue' def key = liftM (fromMaybe def) (getCacheValue key)
