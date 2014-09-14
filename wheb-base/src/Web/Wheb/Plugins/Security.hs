{- | Provides middleware and view protection against CSRF attacks.
     To ensure maximum protection, turn on the setting "enable-secure-cookies".
-}

{-# LANGUAGE OverloadedStrings #-}

module Web.Wheb.Plugins.Security where

import Control.Monad
import Network.Wai
import Web.Wheb
import Web.Wheb.Utils
import Web.Wheb.Plugins.Session
import Data.Text as T
import Network.HTTP.Types

-- | A middleware to protect ALL incoming POST requests aginst CSRF, 
--   throwing the handler upon failure
csrfMiddleware :: (MonadIO m) => WhebHandlerT g s m -> WhebMiddleware g s m
csrfMiddleware fail = do
    checkedOut <- csrfPassed
    if checkedOut then return Nothing else liftM Just fail

-- | Takes a handler to throw when CSRF fails and a handler to run when it succeeds
csrfProtect :: (MonadIO m) => WhebHandlerT g s m -> WhebHandlerT g s m -> WhebHandlerT g s m 
csrfProtect fail pass = do
    checkedOut <- csrfPassed
    if checkedOut then pass else fail

-- | CSRF reads a cookie value ("csrf_token") and compares it to either 
--   submitted post data (param "csrf_token") or request header ("X-CSRF-TOKEN")
csrfPassed :: (MonadIO m) => WhebT a b m Bool
csrfPassed = do
    method <- getWithRequest requestMethod
    case method == methodPost of
        False -> return True
        True -> do
            real_token <- getCSRFToken
            mPostTok <- getPOSTParam "csrf-token"
            case mPostTok of
                Just postTok -> return $ real_token == postTok
                Nothing -> do
                    mReqTok <- getRequestHeader "X-CSRF-TOKEN"
                    case mReqTok of
                        Just reqTok -> return $ real_token == reqTok
                        Nothing -> return $ False

-- | This will get or generate and set a new CSRF Token in the Cookies
getCSRFToken :: (MonadIO m) => WhebT a b m T.Text
getCSRFToken = do
    tok <- getCookie "csrf-token"
    case tok of
        Just t -> return t
        Nothing -> do
            newTok <- makeUUID
            setCookie "csrf-token" newTok
            return newTok
