{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Web.Crunch.Plugins.Auth 
  ( AuthUser (..)
  , AuthContainer (..)
  , AuthApp (..)
  , AuthBackend (..)
  , AuthError (..)
  
  , UserKey
  , Password
  , PwHash
  
  , login
  , logout
  , register
  , getCurrentUser
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

import Web.Crunch
import Web.Crunch.Types
import Web.Crunch.Plugins.Session


type UserKey = Text
type Password = Text
type PwHash = Text

data AuthError = DuplicateUsername | UserDoesNotExist | InvalidPassword
  deriving (Show)

data AuthUser = AuthUser 
  { uniqueUserKey :: UserKey
  } deriving (Show)

data AuthContainer = forall r. AuthBackend r => AuthContainer r

class SessionApp a => AuthApp a where
  getAuthContainer :: a -> AuthContainer

class AuthBackend c where
  backendLogin :: (AuthApp a, MonadIO m) => SessionApp a => UserKey -> Password -> c -> CrunchT a b m (Either AuthError AuthUser)
  backendRegister :: (AuthApp a, MonadIO m) => UserKey -> Password -> c -> CrunchT a b m (Either AuthError AuthUser)
  backendGetUser :: (AuthApp a, MonadIO m) => UserKey -> c -> CrunchT a b m (Maybe AuthUser)
  backendLogout :: (AuthApp a, MonadIO m) => c -> CrunchT a b m ()
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

runWithContainer :: (AuthApp a, MonadIO m) =>
                    (forall r. AuthBackend r => r -> CrunchT a s m b) -> 
                    CrunchT a s m b
runWithContainer f = do
  AuthContainer authStore <- getWithApp getAuthContainer
  f authStore

getUserSessionKey :: (AuthApp a, MonadIO m) => CrunchT a b m Text
getUserSessionKey = return $ T.pack "user-id" -- later read from settings.

register :: (AuthApp a, MonadIO m) => UserKey -> Password -> CrunchT a b m (Either AuthError AuthUser)
register un pw = runWithContainer $ backendRegister un pw

login :: (AuthApp a, MonadIO m) => UserKey -> Password -> CrunchT a b m (Either AuthError AuthUser)
login un pw = do
  loginResult <- (runWithContainer $ backendLogin un pw)
  case loginResult of
      Right au@(AuthUser userKey) -> do
          sessionKey <- getUserSessionKey
          deleteSessionValue sessionKey
          setSessionValue sessionKey userKey
      _ -> return ()
  return loginResult

logout :: (AuthApp a, MonadIO m) => CrunchT a b m ()
logout = runWithContainer $ backendLogout

getCurrentUser :: (AuthApp a, MonadIO m) => CrunchT a b m (Maybe AuthUser)
getCurrentUser = getUserSessionKey >>= getSessionValue' (T.pack "") >>=
                            (\uid -> runWithContainer $ backendGetUser uid)

loginRequired :: CrunchT a b m () -> CrunchT a b m ()
loginRequired action = action

makePwHash :: MonadIO m => Password -> CrunchT a b m PwHash
makePwHash pw = liftM (T.fromStrict . ES.decodeUtf8) $ 
                        liftIO $ makePassword (ES.encodeUtf8 $ T.toStrict pw) 14

verifyPw :: Text -> Text -> Bool
verifyPw pw hash = verifyPassword (ES.encodeUtf8 $ T.toStrict pw) 
                          (ES.encodeUtf8 $ T.toStrict hash)