{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Wheb.Plugins.Cache
  ( CacheApp (..)
  , CacheBackend (..)
  
  , setCacheValue
  , setCacheValue'
  , getCacheValue
  , getCacheValue'
  , deleteCacheValue
  ) where
    
import           Control.Monad (liftM)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Binary (Binary(..), encode, decodeOrFail)
import           Data.ByteString.Lazy as BS (ByteString)
import           Wheb


class (CacheBackend (CacheAppBackend a)) => CacheApp a where
  type CacheAppBackend a :: *
  getCacheContainer :: a -> CacheAppBackend a

class CacheBackend c where
  backendCachePut    :: (CacheApp a, MonadIO m) => Text -> ByteString -> Integer -> c -> WhebT a b m ()
  backendCacheGet    :: (CacheApp a, MonadIO m) => Text -> c -> WhebT a b m (Maybe ByteString)
  backendCacheDelete :: (CacheApp a, MonadIO m) => Text -> c -> WhebT a b m ()

runWithContainer :: (CacheApp a, MonadIO m) => (CacheAppBackend a -> WhebT a s m b) -> WhebT a s m b
runWithContainer f = do
  cacheStore <- getWithApp getCacheContainer
  f cacheStore

deleteCacheValue :: (CacheApp a, MonadIO m) => Text -> WhebT a b m ()
deleteCacheValue key = runWithContainer $ backendCacheDelete key

-- | Set a cache value with an expiration of an hour
setCacheValue :: (CacheApp a, MonadIO m, Binary a) => Text -> a -> WhebT a b m ()
setCacheValue key content = setCacheValue' key content (60 * 60)

-- | Set a cache value with expiration in seconds
setCacheValue' :: (CacheApp a, MonadIO m, Binary a) => Text -> a -> Integer -> WhebT a b m ()
setCacheValue' key content expr = runWithContainer $ backendCachePut key (encode content) expr

getCacheValue :: (CacheApp a, MonadIO m) => Text -> WhebT a b m (Maybe ByteString)
getCacheValue key = do
    mval <- runWithContainer $ backendCacheGet key
    return $ maybe (Nothing) (either (const Nothing) (\(_, _, a) -> a) . decodeOrFail) mval

getCacheValue' :: (CacheApp a, MonadIO m) => ByteString -> Text -> WhebT a b m ByteString
getCacheValue' def key = liftM (fromMaybe def) (getCacheValue key)
