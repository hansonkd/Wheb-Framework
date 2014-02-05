module Web.Wheb.Plugins.Debug.MemoryBackend where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Text.Lazy (Text)
import Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

import Web.Wheb
import Web.Wheb.Types
import Web.Wheb.Plugins.Auth
import Web.Wheb.Plugins.Session

data SessionData = SessionData 
  { sessionMemory ::  TVar (M.Map Text (M.Map Text Text)) }
data UserData = UserData
  { userStorage :: TVar (M.Map UserKey PwHash) }
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

-- | In memory auth backend. User values 
-- will not persist after server restart.
instance AuthBackend UserData where
  backendGetUser name (UserData tv) = do
        possUser <- liftM (M.lookup name) $ liftIO $ readTVarIO tv
        case possUser of
          Nothing -> return Nothing
          Just _ -> return $ Just (AuthUser name)
  backendLogin name pw (UserData tv) = do
        users <- liftIO $ readTVarIO $ tv
        let possUser = M.lookup name users
            passCheck = fmap (verifyPw pw) possUser
        case passCheck of
            Just True -> return (Right $ AuthUser $ name)
            Just False -> return (Left InvalidPassword)
            Nothing -> return (Left UserDoesNotExist)
  backendRegister name pw (UserData tv) = do
        users <- liftIO $ readTVarIO $ tv
        if M.member name users
            then return (Left DuplicateUsername)
            else do
                pwHash <- makePwHash pw
                liftIO $ atomically $ writeTVar tv (M.insert name pwHash users)
                return (Right $ AuthUser name)
                
initSessionMemory :: InitM g s m SessionContainer
initSessionMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! SessionContainer $ SessionData tv

initAuthMemory :: InitM g s m AuthContainer
initAuthMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! AuthContainer $ UserData tv