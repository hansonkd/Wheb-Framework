{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Wheb.Plugins.Auth
  (
  -- * Main functions
    login
  , logout
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
  , AuthCredentials (..)
  , AuthTypes (..)
  , UserId (..)
  , UserKeyType (..)
  , UserKey
  , Password
  , PwHash
  , HostName
  , TOTP
  , Token
  , DeviceToken

  -- * Utils
  , makePwHash
  , verifyPw
  , getUserSessionKey
  ) where


import Control.Monad.Error (liftM, MonadError(throwError), MonadIO(..))
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Text as T (pack, Text, empty)
import Data.Text.Encoding as ES (decodeUtf8, encodeUtf8)
import Wheb (getHandlerState, getWithApp, modifyHandlerState',
                 WhebError(Error403), WhebHandlerT, WhebMiddleware, WhebT)
import Wheb.Plugins.Session (deleteSessionValue, getSessionValue', SessionApp, setSessionValue)


type UserKey = Text
type Password = Text
type PwHash = Text
type HostName = Text
type TOTP = Text
type Token = Text
type DeviceToken = Token

-- | Example for AuthError
data AuthError = UserDoesNotExist | InvalidPassword | NeedPassword | InvalidTOTP
               | NeedTOTP | AuthInternalError | AuthenticationDenied
               deriving (Show, Eq)

data AccountRegError = InvalidEmail | PasswordTooShort | DuplicateUsername | AuthRegInternalError
                     | AuthRegUserDoesNotExist
                  deriving (Show, Eq)


data UserKeyType = EmailKey | UserName deriving (Show)
data UserId = UserId T.Text (Maybe UserKeyType) deriving (Show)

data AuthUser a = AuthUser { userId :: UserId
                         } deriving (Show)

-- | Authentication credentials are things that a client can provide
-- to the authentication backend to support the fact that they are who
-- they claim to be.
data PasswordCredentials =
    PasswordCredentials Password -- ^ A password
    deriving (Show)

data AuthContainer = forall r. AuthBackend r => AuthContainer r


-- | Interface for creating Auth backends
class AuthApp a where
  getAuthContainer :: a -> AuthContainer

-- | Types related to an authentication system.  By default there are
-- no claims, but this can be overridden in the instance declaration.
class WebAppAuthTypes a where
  type Credentials :: *
  type Credentials = PasswordCredentials

  type AuthError' :: *
  type AuthError' = AuthError

  type RegError' :: *
  type RegError' = RegError

-- | Minimal implementation for auth state (typically in a request state).
class AuthTypes a => AuthState a where



-- | Interface for Auth backends.  The login and logout functionality
-- requires that the app is a 'SessionApp' instance.
class AuthTypes c => AuthBackend c where
  -- | Authenticate the user for a given authoriztion using a username and password
  backendLogin :: (AuthApp g, MonadIO m) => UserId -> Credentials -> c -> WhebT g s m (Either AuthError (AuthUser Claims))
  -- | Lookup a user.
  backendGetUser  :: (AuthApp g, MonadIO m) => UserId -> c -> WhebT g s m (Maybe (AuthUser Claims))
  -- | Notification to the backend that a user was logged out.  The
  -- backend is not responsible for clearing the session.  Use the
  -- 'logout' function instead.
  backendLogout   :: (AuthApp g, SessionApp g, MonadIO m) => c -> WhebT g s m ()
  backendLogout _ = return ()


-- * Internal

runWithContainer :: (AuthApp g, MonadIO m) =>
                    (forall r. AuthBackend r => r -> WhebT g s m b) ->
                    WhebT g s m b
runWithContainer f = do
  AuthContainer authStore <- getWithApp getAuthContainer
  f authStore

authSetUser :: (AuthApp g, AuthState s, MonadIO m) => Maybe (AuthUser Claims) -> WhebT g s m ()
authSetUser cur = modifyHandlerState' (modifyAuthUser (const cur))

getUserSessionKey :: (AuthApp g, MonadIO m) => WhebT g s m Text
getUserSessionKey = return $ T.pack "user-id" -- later read from settings.

makePwHash :: MonadIO m => Password -> WhebT g s m PwHash
makePwHash pw = liftM ES.decodeUtf8 $
                        liftIO $ makePassword (ES.encodeUtf8 pw) 14

verifyPw :: Text -> Text -> Bool
verifyPw pw hash = verifyPassword (ES.encodeUtf8 pw)
                          (ES.encodeUtf8 hash)

-- * Auth functions

-- | Register a user
register :: (AccountApp a, MonadIO m) => UserInfo -> Credentials -> WhebT a b m (Either AccountRegError' UserId)
register ui uinf cred = runWithContainer $ backendRegister ui uinf cred

-- | Log a user in
login :: (AuthApp g, SessionApp g, AuthState s, MonadIO m) => UserId -> Claims -> AuthCredentials -> WhebT g s m (Either AuthError (AuthUser Claims))
login ui cl ac = do
  loginResult <- runWithContainer $ backendLogin ui cl ac
  case loginResult of
      Right au@(AuthUser (UserId key _type) _maybeClaims) -> do
          sessionKey <- getUserSessionKey
          deleteSessionValue sessionKey
          setSessionValue sessionKey key
          authSetUser (Just au)
      _ -> return ()
  return loginResult

-- | Log a user out
logout :: (AuthApp g, SessionApp g, AuthState s, MonadIO m) => WhebT g s m ()
logout = do
  getUserSessionKey >>= deleteSessionValue
  authSetUser Nothing

-- | Get the current user from the handler state (Needs to be populated first
--   with 'authMiddleware')
getCurrentUser :: (AuthState s, MonadIO m) => WhebT g s m (Maybe (AuthUser Claims))
getCurrentUser = liftM getAuthUser getHandlerState

-- | Explicitly query a user with the backend. Since this is an IO hit, it is
--   better to use the middleware and 'getCurrentUser'
queryCurrentUser :: (AuthApp g, SessionApp g, MonadIO m) => WhebT g s m (Maybe (AuthUser Claims))
queryCurrentUser = getUserSessionKey >>=
                 getSessionValue' T.empty >>=
                 (\uid -> runWithContainer $ backendGetUser (UserId uid Nothing))

-- | Checks if a user is logged in with 'getCurrentUser' and throws a 500
--   if they aren't.
loginRequired :: (AuthState s, MonadIO m) =>
                 WhebHandlerT g s m ->
                 WhebHandlerT g s m
loginRequired action = getCurrentUser >>=
                       maybe (throwError Error403) (const action)

-- * Middleware

-- | Auto-populates the handler state with the current user.
authMiddleware :: (AuthApp g, SessionApp g, AuthState s, MonadIO m) => WhebMiddleware g s m
authMiddleware = do
    cur <- queryCurrentUser
    authSetUser cur
    return Nothing
