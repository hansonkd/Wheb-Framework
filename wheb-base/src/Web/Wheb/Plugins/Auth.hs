{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Web.Wheb.Plugins.Auth 
  ( 
  -- * Main functions
    login
  , logout
  , register
  , getCurrentUser
  , queryCurrentUser
  , loginRequired
  
  -- * Middleware
  , authMiddleware 
  
  -- * Types
  , AuthUser (..)
  , AuthContainer (..)
  , AuthApp (..)
  , AuthState (..)
  , AuthBackend (..)
  , AuthError (..)

  , UserKey
  , Password
  , PwHash
  
  -- * Utils
  , makePwHash
  , verifyPw
  , getUserSessionKey
  ) where

import Control.Monad.Except (liftM, MonadError(throwError), MonadIO(..))
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Text.Encoding as ES (decodeUtf8, encodeUtf8)
import Data.Text as T (pack, Text)
import Web.Wheb (getHandlerState, getWithApp, modifyHandlerState', 
                 WhebError(Error403), WhebHandlerT, WhebMiddleware, WhebT)
import Web.Wheb.Plugins.Session (deleteSessionValue, getSessionValue', SessionApp, setSessionValue)
    
-- * Auth functions

-- | Register a user
register :: (AuthApp a, MonadIO m) => AuthUser -> Password -> WhebT a b m (Either AuthError AuthUser)
register un pw = runWithContainer $ backendRegister un pw

-- | Log a user in
login :: (AuthApp a, AuthState b, MonadIO m) => UserKey -> Password -> WhebT a b m (Either AuthError AuthUser)
login un pw = do
  loginResult <- (runWithContainer $ backendLogin un pw)
  case loginResult of
      Right au@(AuthUser userKey) -> do
          sessionKey <- getUserSessionKey
          deleteSessionValue sessionKey
          setSessionValue sessionKey userKey
          authSetUser (Just au)
      _ -> return ()
  return loginResult

-- | Log a user out
logout :: (AuthApp a, AuthState b, MonadIO m) => WhebT a b m ()
logout = (runWithContainer backendLogout) >> (authSetUser Nothing)

-- | Get the current user from the handler state (Needs to be populated first
--   with 'authMiddleware')
getCurrentUser :: (AuthState b, MonadIO m) => WhebT a b m (Maybe AuthUser)
getCurrentUser = liftM getAuthUser getHandlerState

-- | Explicitly query a user with the backend. Since this is an IO hit, it is
--   better to use the middleware and 'getCurrentUser'
queryCurrentUser :: (AuthApp a, MonadIO m) => WhebT a b m (Maybe AuthUser)
queryCurrentUser = getUserSessionKey >>= 
                 getSessionValue' (T.pack "") >>=
                 (\uid -> runWithContainer $ backendGetUser uid)

-- | Checks if a user is logged in with 'getCurrentUser' and throws a 500
--   if they aren't.
loginRequired :: (AuthState b, MonadIO m) =>
                 WhebHandlerT a b m ->
                 WhebHandlerT a b m
loginRequired action = getCurrentUser >>=
                       (maybe (throwError Error403) (const action))

-- * Middleware

-- | Auto-populates the handler state with the current user.
authMiddleware :: (AuthApp a, AuthState b, MonadIO m) => WhebMiddleware a b m
authMiddleware = do
    cur <- queryCurrentUser
    authSetUser cur
    return Nothing

type UserKey = Text
type Password = Text
type PwHash = Text

data AuthError = DuplicateUsername | UserDoesNotExist | InvalidPassword
              |  InternalError
  deriving (Show)

data AuthUser = AuthUser { uniqueUserKey :: UserKey } deriving (Show)

type PossibleUser = Maybe AuthUser

data AuthContainer = forall r. AuthBackend r => AuthContainer r

-- | Interface for creating Auth backends
class SessionApp a => AuthApp a where
  getAuthContainer :: a -> AuthContainer

-- | Minimal implementation for a 
class AuthState a where
  getAuthUser    :: a -> PossibleUser 
  modifyAuthUser :: (PossibleUser -> PossibleUser) -> a -> a

-- | Interface for creating Auth backends
class AuthBackend c where
  backendLogin    :: (AuthApp a, MonadIO m) => SessionApp a => UserKey -> Password -> c -> WhebT a b m (Either AuthError AuthUser)
  backendRegister :: (AuthApp a, MonadIO m) => AuthUser -> Password -> c -> WhebT a b m (Either AuthError AuthUser)
  backendGetUser  :: (AuthApp a, MonadIO m) => UserKey -> c -> WhebT a b m (Maybe AuthUser)
  backendLogout   :: (AuthApp a, MonadIO m) => c -> WhebT a b m ()
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue
  
-- * Internal

runWithContainer :: (AuthApp a, MonadIO m) =>
                    (forall r. AuthBackend r => r -> WhebT a s m b) -> 
                    WhebT a s m b
runWithContainer f = do
  AuthContainer authStore <- getWithApp getAuthContainer
  f authStore

authSetUser :: (AuthApp a, AuthState b, MonadIO m) => PossibleUser -> WhebT a b m ()
authSetUser cur = modifyHandlerState' (modifyAuthUser (const cur))

getUserSessionKey :: (AuthApp a, MonadIO m) => WhebT a b m Text
getUserSessionKey = return $ T.pack "user-id" -- later read from settings.

makePwHash :: MonadIO m => Password -> WhebT a b m PwHash
makePwHash pw = liftM (ES.decodeUtf8) $ 
                        liftIO $ makePassword (ES.encodeUtf8 $ pw) 14

verifyPw :: Text -> Text -> Bool
verifyPw pw hash = verifyPassword (ES.encodeUtf8 $ pw) 
                          (ES.encodeUtf8 $ hash)
