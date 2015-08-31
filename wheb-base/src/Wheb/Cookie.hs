{-# LANGUAGE OverloadedStrings #-}

module Wheb.Cookie
  ( setCookie
  , setCookie'
  , getCookie
  , getCookies
  , removeCookie
  ) where

import           Control.Monad (liftM)
import qualified Blaze.ByteString.Builder as B (toByteString)
import           Control.Monad.State (modify', MonadState(get))
import           Data.Text (Text)
import qualified Data.Text as TS (empty)
import qualified Data.Text.Encoding as TS (encodeUtf8)
import           Data.Time.Calendar (Day(ModifiedJulianDay))
import           Data.Time.Clock (secondsToDiffTime, UTCTime(UTCTime))
import           Web.Cookie (CookiesText, def, renderSetCookie, SetCookie(..))
import           Wheb.WhebT (setRawHeader, getSetting'')
import           Wheb.Types

getDefaultCookie :: Monad m => WhebT g s m SetCookie
getDefaultCookie = return def -- Populate with settings...

setCookie :: Monad m => Text -> Text -> WhebT g s m ()
setCookie k v = getDefaultCookie >>= setCookie' k v

-- | Set a cookie. Looks up setting "enable-secure-cookies" to control turning
--  HTTPS only cookies on. This should be enabled on production environments.
setCookie' :: Monad m => Text -> Text -> SetCookie -> WhebT g s m ()
setCookie' k v sc = do
  secureCookie <- getSetting'' "enable-secure-cookies" False
  let cookie = sc { setCookieName = TS.encodeUtf8 k
                  , setCookieValue = TS.encodeUtf8 v
                  , setCookieHttpOnly = secureCookie
                  , setCookieSecure = secureCookie
                  }
      cookieText = B.toByteString $ renderSetCookie cookie
  setRawHeader ("Set-Cookie", cookieText)
  WhebT $ modify' (\a -> a {curCookies = (k,v) : curCookies a})

getCookies :: Monad m => WhebT g s m CookiesText
getCookies = WhebT $ liftM curCookies get

getCookie :: Monad m => Text -> WhebT g s m (Maybe Text)
getCookie k = liftM (lookup k) getCookies

removeCookie :: Monad m => Text -> WhebT g s m ()
removeCookie k = do
  defCookie <- getDefaultCookie
  let utcLongAgo = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      expiredCookie = defCookie {setCookieExpires = Just utcLongAgo}
  setCookie' k TS.empty expiredCookie
