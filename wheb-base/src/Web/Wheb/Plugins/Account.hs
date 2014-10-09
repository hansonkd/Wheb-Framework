{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Account plugin.  This is a companion to the Auth plugin.  This
-- interface provides support for aministering accounts, such as
-- registering and unregistering.  Simple username/password,
-- email/password, and email only registrations are supported by
-- default, but a backend can specify richer 'UserInfo', 'Credentials'
-- and account registration error types ('AccountRegEror') using type families.
--
-- Using these types, a user can be registered with specific account
-- information as well as with specific credentials compatible with
-- the 'Auth' plugin.

module Web.Wheb.Plugins.Account where

import           Control.Monad.Error (MonadIO(..))
import qualified Data.Text as T (Text, empty)
import           Web.Wheb (getWithApp, WhebT)
import           Web.Wheb.Plugins.Auth

-- * Account functions

-- | Register a user
register :: (AccountApp a, MonadIO m) => UserId -> UserInfo -> Credentials -> WhebT a b m (Either AccountRegError' UserId)
register ui uinf cred = runWithContainer $ backendRegister ui uinf cred

-- | Register a user
registerByUsernamePassword :: (AccountApp a, MonadIO m) => UserKey -> Password -> WhebT a b m (Either AccountRegError' UserId)
registerByUsernamePassword un pw = runWithContainer $
                                   backendRegisterByUsernamePassword un pw

-- | Register a user
registerByEmailPassword :: (AccountApp a, MonadIO m) => T.Text -> Password -> WhebT a b m (Either AccountRegError' UserId)
registerByEmailPassword un pw = runWithContainer $ backendRegisterByEmailPassword un pw

-- | Register interest, but not a full user
registerByEmail :: (AccountApp a, MonadIO m) => T.Text -> WhebT a b m (Either AccountRegError' UserId)
registerByEmail ek = runWithContainer $ backendRegisterByEmail ek


-- * Account Middleware

-- TODO: Reuse PasswordFeature from 'passwords' for reporting password
-- feature erors?  PasswordError [PasswordFeature]
data AccountRegError = InvalidEmail | PasswordTooShort | DuplicateUsername | AuthRegInternalError
                     -- AuthRegUserDoesNotExist is kind of shared with
                     -- AuthError, which is a shame.  Refactor
                     -- somehow?
                     | AuthRegUserDoesNotExist
                  deriving (Show, Eq)


data AccountContainer = forall r. AccountBackend r => AccountContainer r

-- | Interface for creating Auth backends
class AccountApp a where
  getAccountContainer :: a -> AccountContainer

-- | Interface for registering and modifying accounts
class AuthTypes c => AccountBackend c where
  type UserInfo :: *
  type UserInfo = ()
  type AccountRegError' :: *
  type AccountRegError' = AccountRegError
  -- | Register a new user using the instance-specific types 'UserInfo' and 'Credentials'
  backendRegister :: (AccountApp a, MonadIO m) => UserId -> UserInfo -> Credentials -> c -> WhebT a b m (Either AccountRegError' UserId)

  -- TODO: Add some type trickery to implement default versions for
  -- instances using default UserInfo and Credentials.

  -- | Register a new user using a username and a password
  backendRegisterByUsernamePassword :: (AccountApp a, MonadIO m) => T.Text -> Password -> c -> WhebT a b m (Either AccountRegError' UserId)
  -- | Register a new user using an email and a password
  backendRegisterByEmailPassword :: (AccountApp a, MonadIO m) => T.Text -> Password -> c -> WhebT a b m (Either AccountRegError' UserId)
  -- | Register a new user using an email address only
  backendRegisterByEmail :: (AccountApp a, MonadIO m) => T.Text -> c -> WhebT a b m (Either AccountRegError' UserId)
  backendRegisterByEmail em = backendRegisterByEmailPassword em T.empty
  backendUnregister :: (AccountApp a, MonadIO m) => UserId -> c -> WhebT a b m (Either AccountRegError' ())

-- * Utilities when writing an account backend

-- | Define monoid?
emptyCreds :: AuthCredentials
emptyCreds = AuthCredentials Nothing Nothing Nothing Nothing Nothing

credsByPw :: Password -> AuthCredentials
credsByPw pw = AuthCredentials (Just pw) Nothing Nothing Nothing Nothing

credsByToken :: Token -> AuthCredentials
credsByToken t = AuthCredentials Nothing Nothing Nothing Nothing (Just t)


-- * Internal

runWithContainer :: (AccountApp a, MonadIO m) =>
                    (forall r. AccountBackend r => r -> WhebT a s m b) ->
                    WhebT a s m b
runWithContainer f = do
  AccountContainer accountStore <- getWithApp getAccountContainer
  f accountStore
