{-# LANGUAGE OverloadedStrings #-}

module Web.Wheb.Cookie
  ( setCookie
  , setCookie'
  , getCookie
  , getCookies
  , removeCookie
  ) where

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as B

import Web.Cookie

import Web.Wheb
import Web.Wheb.Types

getDefaultCookie :: Monad m => WhebT g s m SetCookie
getDefaultCookie = return def -- Populate with settings...

setCookie :: Monad m => Text -> Text -> WhebT g s m ()
setCookie k v = getDefaultCookie >>= (setCookie' k v)

setCookie' :: Monad m => Text -> Text -> SetCookie -> WhebT g s m ()
setCookie' k v sc = setRawHeader ("Set-Cookie", cookieText)
  where cookie = sc { setCookieName = lazyTextToSBS k
                    , setCookieValue = lazyTextToSBS v
                    }
        cookieText = B.toByteString $ renderSetCookie cookie
        
getCookies :: Monad m => WhebT g s m CookiesText
getCookies = (getHeader "Cookie") >>= (return . parseFunc . (fromMaybe T.empty))
  where parseFunc = parseCookiesText . lazyTextToSBS

getCookie :: Monad m => Text -> WhebT g s m (Maybe Text)
getCookie k = getCookies >>= 
    (return . (fmap T.fromStrict) . (lookup (T.toStrict k)))

removeCookie :: Monad m => Text -> WhebT g s m ()
removeCookie k = do
  defCookie <- getDefaultCookie
  let utcLongAgo = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      expiredCookie = defCookie {setCookieExpires = Just utcLongAgo}
  setCookie' k T.empty expiredCookie