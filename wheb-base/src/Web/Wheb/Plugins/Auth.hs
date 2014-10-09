{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Web.Wheb.Plugins.Auth
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
import Web.Wheb (getHandlerState, getWithApp, modifyHandlerState', WhebError(Error403), WhebHandlerT, WhebMiddleware, WhebT)
import Web.Wheb.Plugins.Session (deleteSessionValue, getSessionValue', SessionApp, setSessionValue)

-- * Auth functions

-- | Log a user in
login :: (AuthApp a, SessionApp a, AuthState b, MonadIO m) => UserId -> Claims -> Credentials -> WhebT a b m (Either AuthError' (AuthUser Claims))
login ui cl ac = do
  loginResult <- (runWithContainer $ backendLogin ui cl ac)
  case loginResult of
      Right au@(AuthUser (UserId key _type) _maybeClaims) -> do
          sessionKey <- getUserSessionKey
          deleteSessionValue sessionKey
          setSessionValue sessionKey key
          authSetUser (Just au)
      _ -> return ()
  return loginResult

-- | Log a user out
logout :: (AuthApp a, SessionApp a, AuthState b, MonadIO m) => WhebT a b m ()
logout = do
  getUserSessionKey >>= deleteSessionValue
  authSetUser Nothing

-- | Get the current user from the handler state (Needs to be populated first
--   with 'authMiddleware')
getCurrentUser :: (AuthState b, MonadIO m) => WhebT a b m (Maybe (AuthUser Claims))
getCurrentUser = liftM getAuthUser getHandlerState

-- | Explicitly query a user with the backend. Since this is an IO hit, it is
--   better to use the middleware and 'getCurrentUser'
queryCurrentUser :: (AuthApp a, SessionApp a, MonadIO m) => WhebT a b m (Maybe (AuthUser Claims))
queryCurrentUser = getUserSessionKey >>=
                 getSessionValue' T.empty >>=
                 (\uid -> runWithContainer $ backendGetUser (UserId uid Nothing))

-- | Checks if a user is logged in with 'getCurrentUser' and throws a 500
--   if they aren't.
loginRequired :: (AuthState b, MonadIO m) =>
                 WhebHandlerT a b m ->
                 WhebHandlerT a b m
loginRequired action = getCurrentUser >>=
                       (maybe (throwError Error403) (const action))

-- * Middleware

-- | Auto-populates the handler state with the current user.
authMiddleware :: (AuthApp a, SessionApp a, AuthState b, MonadIO m) => WhebMiddleware a b m
authMiddleware = do
    cur <- queryCurrentUser
    authSetUser cur
    return Nothing

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

data UserKeyType = EmailKey | UserName deriving (Show)
data UserId = UserId T.Text (Maybe UserKeyType) deriving (Show)

data AuthUser a = AuthUser { userId :: UserId
                             -- | The authorizations of the
                             -- authenticated user. This can be
                             -- based on the credentials provided
                             -- (simple password, known device, 2FA).
                           , claims :: a
                         } deriving (Show)

-- | Authentication credentials are things that a client can provide
-- to the authentication backend to support the fact that they are who
-- they claim to be.
data AuthCredentials = AuthCredentials { acredPassword :: Maybe Password  -- ^ A password
                                       , acredHostName :: Maybe HostName  -- ^ The IP of the client
                                       , acredTOTP :: Maybe TOTP      -- ^ RFC 6238 2FA token
                                       , acredDeviceToken :: Maybe DeviceToken -- ^ JWT token or cookie identifying the device.
                                       , acredToken :: Maybe Token     -- ^ JWT token (expired or not)
                                       } deriving (Show)

data AuthContainer = forall r. AuthBackend r => AuthContainer r


-- | Interface for creating Auth backends
class AuthApp a where
  getAuthContainer :: a -> AuthContainer

-- | Types related to an authentication system.  By default there are
-- no claims, but this can be overridden in the instance declaration.
class AuthTypes a where
  type Claims :: *     -- ^ The representation for claims
  type Claims = ()
  type Credentials :: *
  type Credentials = AuthCredentials

  type AuthError' :: *
  type AuthError' = AuthError

-- | Minimal implementation for auth state (typically in a request state).
class AuthTypes a => AuthState a where
  getAuthUser    :: a -> Maybe (AuthUser Claims)
  modifyAuthUser :: (Maybe (AuthUser Claims) -> Maybe (AuthUser Claims)) -> a -> a


-- | Interface for Auth backends.  The login and logout functionality
-- requires that the app is a 'SessionApp' instance.
class AuthTypes c => AuthBackend c where
  -- | Authenticate the user for a given authoriztion using a username and password
  backendLogin :: (AuthApp a, MonadIO m) => UserId -> Claims -> Credentials -> c -> WhebT a b m (Either AuthError' (AuthUser Claims))
  -- | Lookup a user.
  backendGetUser  :: (AuthApp a, MonadIO m) => UserId -> c -> WhebT a b m (Maybe (AuthUser Claims))
  -- | Notification to the backend that a user was logged out.  The
  -- backend is not responsible for clearing the session.  Use the
  -- 'logout' function instead.
  backendLogout   :: (AuthApp a, SessionApp a, MonadIO m) => c -> WhebT a b m ()
  backendLogout _ = return ()

-- * Internal

runWithContainer :: (AuthApp a, MonadIO m) =>
                    (forall r. AuthBackend r => r -> WhebT a s m b) ->
                    WhebT a s m b
runWithContainer f = do
  AuthContainer authStore <- getWithApp getAuthContainer
  f authStore

authSetUser :: (AuthApp a, AuthState b, MonadIO m) => Maybe (AuthUser Claims) -> WhebT a b m ()
authSetUser cur = modifyHandlerState' (modifyAuthUser (const cur))

getUserSessionKey :: (AuthApp a, MonadIO m) => WhebT a b m Text
getUserSessionKey = return $ T.pack "user-id" -- later read from settings.

makePwHash :: MonadIO m => Password -> WhebT a b m PwHash
makePwHash pw = liftM (ES.decodeUtf8) $
                        liftIO $ makePassword (ES.encodeUtf8 $ pw) 14

verifyPw :: Text -> Text -> Bool
verifyPw pw hash = verifyPassword (ES.encodeUtf8 $ pw)
                          (ES.encodeUtf8 $ hash)
