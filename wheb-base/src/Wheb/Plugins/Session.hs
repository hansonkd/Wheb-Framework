{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Wheb.Plugins.Session 
  ( SessionApp (..)
  , SessionBackend (..)
  
  , setSessionValue
  , getSessionValue
  , getSessionValue'
  , deleteSessionValue
  , generateSessionKey
  , getCurrentSessionKey
  , clearSessionKey
  ) where
    
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Binary (Binary(..), encode, decodeOrFail)
import           Data.Maybe (fromMaybe)
import           Data.Text (pack, Text)
import           Data.ByteString.Lazy (ByteString)
import           Wheb (getWithApp, WhebT)
import           Wheb.Cookie (getCookie, setCookie)
import           Wheb.Utils (makeUUID)

-- | Initial pass on abstract plugin for Sessions.
--   Possibly add support for Typable to ease typecasting.

sessionCookieKey :: Text
sessionCookieKey = pack "-session-"

class (SessionBackend (SessionAppBackend a)) => SessionApp a where
  type SessionAppBackend a :: *
  getSessionContainer :: a -> SessionAppBackend a

class SessionBackend c where
  backendSessionPut    :: (SessionApp a, MonadIO m) => Text -> Text -> ByteString -> c -> WhebT a b m ()
  backendSessionGet    :: (SessionApp a, MonadIO m) => Text -> Text -> c -> WhebT a b m (Maybe ByteString)
  backendSessionDelete :: (SessionApp a, MonadIO m) => Text -> Text -> c -> WhebT a b m ()
  backendSessionClear  :: (SessionApp a, MonadIO m) => Text -> c -> WhebT a b m ()

runWithContainer :: (SessionApp a, MonadIO m) => (SessionAppBackend a -> WhebT a s m b) -> WhebT a s m b
runWithContainer f = do
  sessStore <- getWithApp getSessionContainer
  f sessStore

deleteSessionValue :: (SessionApp a, MonadIO m) => Text -> WhebT a b m ()
deleteSessionValue key = do
      sessId <- getCurrentSessionKey 
      runWithContainer $ backendSessionDelete sessId key

setSessionValue :: (SessionApp a, MonadIO m, Binary a) => Text -> a -> WhebT a b m ()
setSessionValue key content = do
      sessId <- getCurrentSessionKey 
      runWithContainer $ backendSessionPut sessId key (encode content)

getSessionValue :: (SessionApp a, MonadIO m, Binary a) => Text -> WhebT a b m (Maybe a)
getSessionValue key = do
      sessId <- getCurrentSessionKey
      mval <- runWithContainer $ backendSessionGet sessId key
      return $ maybe (Nothing) (either (const Nothing) (\(_, _, a) -> a) . decodeOrFail) mval

getSessionValue' :: (SessionApp a, MonadIO m, Binary a) => a -> Text -> WhebT a b m a
getSessionValue' def key = liftM (fromMaybe def) (getSessionValue key)
      
getSessionCookie :: (SessionApp a, MonadIO m) => WhebT a b m (Maybe Text)
getSessionCookie = getCookie sessionCookieKey

generateSessionKey :: (SessionApp a, MonadIO m) => WhebT a b m Text
generateSessionKey = do
  newKey <- makeUUID
  setCookie sessionCookieKey newKey
  return newKey

getCurrentSessionKey :: (SessionApp a, MonadIO m) => WhebT a b m Text
getCurrentSessionKey = do
    curKey <- getSessionCookie
    case curKey of
      Just key -> return key
      Nothing -> generateSessionKey

clearSessionKey :: (SessionApp a, MonadIO m) => WhebT a b m Text
clearSessionKey = do
    curKey <- getSessionCookie
    newKey <- generateSessionKey
    case curKey of
      Nothing -> return newKey
      Just oldKey -> do
          runWithContainer $ backendSessionClear oldKey
          return newKey