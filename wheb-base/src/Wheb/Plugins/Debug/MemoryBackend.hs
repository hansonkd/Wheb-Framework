module Wheb.Plugins.Debug.MemoryBackend where

import           Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO, TVar, writeTVar)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Except (runExceptT, throwE)
import           Data.Map as M (alter, delete, empty, insert, lookup, Map, member, update)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as LS (ByteString)

import           Wheb (InitM)
import           Wheb.Plugins.Cache
import           Wheb.Plugins.Session

data SessionData = SessionData
  { sessionMemory ::  TVar (M.Map Text (M.Map Text LS.ByteString)) }

data CacheData = CacheData
  { cacheStorage :: TVar (M.Map Text LS.ByteString) }


-- | In memory cache backend. Cache value
-- will not persist after server restart and will never clear old values.
instance CacheBackend CacheData where
  backendCachePut key content _ (CacheData tv) =
    liftIO $ atomically $ modifyTVar tv (M.insert key content)
  backendCacheGet key (CacheData tv) = do
      curCache <- liftIO $ readTVarIO tv
      return $ M.lookup key curCache
  backendCacheDelete key (CacheData tv) =
      liftIO $ atomically $ modifyTVar tv (M.delete key)

-- | In memory session backend. Session values
-- will not persist after server restart.
instance SessionBackend SessionData where
  backendSessionPut sessId key content (SessionData tv) =
      let insertFunc sess = Just $ M.insert key content (fromMaybe M.empty sess)
          tVarFunc = M.alter insertFunc sessId
      in liftIO $ atomically $ modifyTVar tv tVarFunc
  backendSessionGet sessId key (SessionData tv) = do
      curSessions <- liftIO $ readTVarIO tv
      return $ M.lookup sessId curSessions >>= M.lookup key
  backendSessionDelete sessId key (SessionData tv) =
      liftIO $ atomically $ modifyTVar tv (M.update (Just . M.delete key) sessId)
  backendSessionClear sessId (SessionData tv) =
      liftIO $ atomically $ modifyTVar tv (M.delete sessId)

initSessionMemory :: InitM g s m SessionData
initSessionMemory = do
  tv <- liftIO $ newTVarIO M.empty
  return $ SessionData tv

initCacheMemory :: InitM g s m CacheData
initCacheMemory = do
  tv <- liftIO $ newTVarIO M.empty
  return $ CacheData tv
