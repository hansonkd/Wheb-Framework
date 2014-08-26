{-# LANGUAGE OverloadedStrings #-}

module Web.Wheb.Cookie
  ( setCookie
  , setCookie'
  , getCookie
  , getCookies
  , removeCookie
  ) where

import Control.Monad (liftM)
import qualified Blaze.ByteString.Builder as B (toByteString)
import Control.Monad.State (modify, MonadState(get))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as TS (empty)
import qualified Data.Text.Encoding as TS (decodeUtf8, encodeUtf8)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (secondsToDiffTime, UTCTime(UTCTime))
import Web.Cookie (CookiesText, def, parseCookiesText, renderSetCookie, 
                   SetCookie(..))
import Web.Wheb.Types
import Web.Wheb.WhebT (getRequestHeader, setRawHeader)

getDefaultCookie :: Monad m => WhebT g s m SetCookie
getDefaultCookie = return def -- Populate with settings...

setCookie :: Monad m => Text -> Text -> WhebT g s m ()
setCookie k v = getDefaultCookie >>= (setCookie' k v)

setCookie' :: Monad m => Text -> Text -> SetCookie -> WhebT g s m ()
setCookie' k v sc = do
  setRawHeader ("Set-Cookie", cookieText)
  WhebT $ modify (\a -> a {curCookies = [(k,v)] ++ (curCookies a)})
  where cookie = sc { setCookieName = TS.encodeUtf8 k
                    , setCookieValue = TS.encodeUtf8 v
                    }
        cookieText = B.toByteString $ renderSetCookie cookie
        
getCookies :: Monad m => WhebT g s m CookiesText
getCookies = WhebT $ liftM (curCookies) get

getCookie :: Monad m => Text -> WhebT g s m (Maybe Text)
getCookie k = getCookies >>= 
    (return  . (lookup k))

removeCookie :: Monad m => Text -> WhebT g s m ()
removeCookie k = do
  defCookie <- getDefaultCookie
  let utcLongAgo = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      expiredCookie = defCookie {setCookieExpires = Just utcLongAgo}
  setCookie' k TS.empty expiredCookie
