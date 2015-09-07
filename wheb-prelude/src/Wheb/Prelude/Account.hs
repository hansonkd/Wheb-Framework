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

module Wheb.Plugins.Account where

import           Control.Monad.Error (MonadIO(..))
import qualified Data.Text as T (Text, empty)
import           Wheb (getWithApp, WhebT)
import           Wheb.Plugins.Auth


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

class AuthTypes a => AccountBackend a where

    -- | Register a new user using the instance-specific types 'UserInfo' and 'Credentials'
    backendRegister :: (AccountApp a, MonadIO m) => UserInfo -> Credentials -> c -> Backend (Either AccountRegError User)
    -- | Register a new user using the instance-specific types 'UserInfo' and 'Credentials'
    backendUnregister :: (AccountApp a, MonadIO m) => UserId -> c -> Backend (Either AccountRegError ())
    -- | Activate a new account
    backendSetActiveStatus :: UserId -> Bool -> Backend (Either AccountRegError ())

-- * Account functions

-- | Register a user

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
