module Web.Wheb.Plugins.Debug.MemoryBackend where

import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO, TVar, writeTVar)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Map as M (alter, delete, empty, insert, lookup, Map, member, update)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Web.Wheb (InitM)
import Web.Wheb.Plugins.Account
import Web.Wheb.Plugins.Auth
import Web.Wheb.Plugins.Cache
import Web.Wheb.Plugins.Session

data SessionData = SessionData
  { sessionMemory ::  TVar (M.Map Text (M.Map Text Text)) }
data UserData = UserData
  { userStorage :: TVar (M.Map UserKey (Maybe PwHash)) }
data CacheData = CacheData
  { cacheStorage :: TVar (M.Map Text ByteString) }


-- | In memory cache backend. Cache value
-- will not persist after server restart and will never clear old values.
instance CacheBackend CacheData where
  backendCachePut key content _ (CacheData tv) = do
    liftIO $ atomically $ modifyTVar tv (M.insert key content)
  backendCacheGet key (CacheData tv) = do
      curCache <- liftIO $ readTVarIO tv
      return $ (M.lookup key curCache)
  backendCacheDelete key (CacheData tv) =
      liftIO $ atomically $ modifyTVar tv (M.delete key)

-- | In memory session backend. Session values
-- will not persist after server restart.
instance SessionBackend SessionData where
  backendSessionPut sessId key content (SessionData tv) =
      let insertFunc = (\sess ->
                          Just $ M.insert key content (fromMaybe M.empty sess)
                       )
          tVarFunc = M.alter insertFunc sessId
      in liftIO $ atomically $ modifyTVar tv tVarFunc
  backendSessionGet sessId key (SessionData tv) = do
      curSessions <- liftIO $ readTVarIO tv
      return $ (M.lookup sessId curSessions) >>= (M.lookup key)
  backendSessionDelete sessId key (SessionData tv) =
      liftIO $ atomically $ modifyTVar tv (M.update (Just . (M.delete key)) sessId)
  backendSessionClear sessId (SessionData tv) =
      liftIO $ atomically $ modifyTVar tv (M.delete sessId)

instance AuthTypes UserData
-- | In memory auth backend. User values
-- will not persist after server restart.
instance AuthBackend UserData where
  backendGetUser ui@(UserId key _type) (UserData tv) = do
        possUser <- liftM (M.lookup key) $ liftIO $ readTVarIO tv
        case possUser of
          Nothing -> return Nothing
          Just _ -> return $ Just (AuthUser ui ())
  backendLogin ui@(UserId key _type) claim (AuthCredentials (Just pw) _ _ _ _) (UserData tv) = do
        users <- liftIO $ readTVarIO $ tv
        runExceptT $ do
          maybePw <- maybe (throwE UserDoesNotExist) return $ M.lookup key users
          if maybe True (verifyPw pw) maybePw
             then return $ AuthUser ui claim
             else throwE InvalidPassword
  backendLogin _ _ _ _ = return $ Left AuthInternalError
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

instance AccountBackend UserData where
  backendRegister ui@(UserId name _) () (AuthCredentials pw _host _totp _dev _tok) (UserData tv) = do
    users <- liftIO $ readTVarIO $ tv
    if M.member name users
      then return $ Left DuplicateUsername
      else do
      maybePwHash <- maybe (return Nothing) (\p -> return . Just =<< makePwHash p) pw
      liftIO $ atomically $ writeTVar tv (M.insert name maybePwHash users)
      return $ Right ui
  backendRegisterByUsernamePassword un pw = backendRegister (UserId un $ Just UserName) () (credsByPw pw)
  backendRegisterByEmailPassword em pw = backendRegister (UserId em $ Just EmailKey) () (credsByPw pw)
  backendRegisterByEmail em               = backendRegister (UserId em $ Just EmailKey) () emptyCreds
  backendUnregister (UserId name _) (UserData tv) = do
    users <- liftIO $ readTVarIO $ tv
    if not $ M.member name users
       then return $ Left AuthRegUserDoesNotExist
       else do
      liftIO $ atomically $ writeTVar tv (M.delete name users)
      return $ Right ()



initSessionMemory :: InitM g s m SessionContainer
initSessionMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! SessionContainer $ SessionData tv

initAuthMemory :: InitM g s m AuthContainer
initAuthMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! AuthContainer $ UserData tv

initCacheMemory :: InitM g s m CacheContainer
initCacheMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! CacheContainer $ CacheData tv
