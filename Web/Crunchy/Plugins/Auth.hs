{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Web.Crunchy.Plugins.Auth 
  ( AuthUser (..)
  , AuthContainer (..)
  , AuthApp (..)
  , AuthState (..)
  , AuthBackend (..)
  , AuthError (..)
  
  , UserKey
  , Password
  , PwHash
  
  , authMiddleware
  , login
  , logout
  , register
  , getCurrentUser
  , queryCurrentUser
  , loginRequired
  , makePwHash
  , verifyPw
  ) where

import Control.Applicative ((<*>))
import Control.Monad (void, liftM)
import Control.Monad.IO.Class
import Crypto.PasswordStore
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as T
import Data.Text.Encoding as ES

import Web.Crunchy
import Web.Crunchy.Types
import Web.Crunchy.Plugins.Session


type UserKey = Text
type Password = Text
type PwHash = Text

data AuthError = DuplicateUsername | UserDoesNotExist | InvalidPassword
  deriving (Show)

data AuthUser = AuthUser { uniqueUserKey :: UserKey } deriving (Show)
type PossibleUser = Maybe AuthUser

data AuthContainer = forall r. AuthBackend r => AuthContainer r

class SessionApp a => AuthApp a where
  getAuthContainer :: a -> AuthContainer

class AuthState a where
  getAuthUser    :: a -> PossibleUser 
  modifyAuthUser :: (PossibleUser -> PossibleUser) -> a -> a

class AuthBackend c where
  backendLogin    :: (AuthApp a, MonadIO m) => SessionApp a => UserKey -> Password -> c -> CrunchyT a b m (Either AuthError AuthUser)
  backendRegister :: (AuthApp a, MonadIO m) => UserKey -> Password -> c -> CrunchyT a b m (Either AuthError AuthUser)
  backendGetUser  :: (AuthApp a, MonadIO m) => UserKey -> c -> CrunchyT a b m (Maybe AuthUser)
  backendLogout   :: (AuthApp a, MonadIO m) => c -> CrunchyT a b m ()
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

runWithContainer :: (AuthApp a, MonadIO m) =>
                    (forall r. AuthBackend r => r -> CrunchyT a s m b) -> 
                    CrunchyT a s m b
runWithContainer f = do
  AuthContainer authStore <- getWithApp getAuthContainer
  f authStore

authSetUser :: (AuthApp a, AuthState b, MonadIO m) => PossibleUser -> CrunchyT a b m ()
authSetUser cur = modifyReqState' (modifyAuthUser (const cur))

authMiddleware :: (AuthApp a, AuthState b, MonadIO m) => CrunchyMiddleware a b m
authMiddleware = do
    cur <- queryCurrentUser
    authSetUser cur
    return Nothing

getUserSessionKey :: (AuthApp a, MonadIO m) => CrunchyT a b m Text
getUserSessionKey = return $ T.pack "user-id" -- later read from settings.

register :: (AuthApp a, MonadIO m) => UserKey -> Password -> CrunchyT a b m (Either AuthError AuthUser)
register un pw = runWithContainer $ backendRegister un pw

login :: (AuthApp a, AuthState b, MonadIO m) => UserKey -> Password -> CrunchyT a b m (Either AuthError AuthUser)
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

logout :: (AuthApp a, AuthState b, MonadIO m) => CrunchyT a b m ()
logout = (runWithContainer backendLogout) >> (authSetUser Nothing)

getCurrentUser :: (AuthState b, MonadIO m) => CrunchyT a b m (Maybe AuthUser)
getCurrentUser = liftM getAuthUser getReqState

queryCurrentUser :: (AuthApp a, MonadIO m) => CrunchyT a b m (Maybe AuthUser)
queryCurrentUser = getUserSessionKey >>= 
                 getSessionValue' (T.pack "") >>=
                 (\uid -> runWithContainer $ backendGetUser uid)

loginRequired :: CrunchyT a b m () -> CrunchyT a b m ()
loginRequired action = action

makePwHash :: MonadIO m => Password -> CrunchyT a b m PwHash
makePwHash pw = liftM (T.fromStrict . ES.decodeUtf8) $ 
                        liftIO $ makePassword (ES.encodeUtf8 $ T.toStrict pw) 14

verifyPw :: Text -> Text -> Bool
verifyPw pw hash = verifyPassword (ES.encodeUtf8 $ T.toStrict pw) 
                          (ES.encodeUtf8 $ T.toStrict hash)