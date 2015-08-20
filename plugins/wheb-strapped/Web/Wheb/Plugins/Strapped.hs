{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wheb.Plugins.Strapped
  ( StrappedContainer(..)
  , StrappedApp (..)
  , renderTemplate
  , initStrapped    
  , module Text.Strapped
  ) where



import           Control.Monad.Except

import           Wheb
import           Wheb.Plugins.Security

import           Text.Strapped
import           Data.Text.Lazy as T
import           Data.Text as TS


data StrappedContainer m = StrappedContainer 
    { renderconfig  :: StrappedConfig 
    , defaultBucket :: InputBucket m
    }

class StrappedApp g m where
    getStrappedContainer :: g -> StrappedContainer m

-- | Load Strapped from a directory matching the extention
initStrapped :: MonadIO m => StrappedConfig -> FilePath -> String -> InitM g s m (StrappedContainer (WhebT g s m))
initStrapped config fp s = do
    mtmpls <- liftIO $ templateStoreFromDirectory config fp s
    case mtmpls of
        Left err -> error (show err)
        Right tmpls -> return $ StrappedContainer (config { templateStore = tmpls }) csrf_bucket

    where csrf_bucket = bucketFromList [ ("csrf_token", Func $ \_ -> lift $ liftM LitText getCSRFToken)
                                       , ("csrf_input", Func $ \_ -> lift $ do 
                                                                        tok <- getCSRFToken
                                                                        return $ LitText $ 
                                                                                "<input type=\"hidden\" name=\"csrf-token\" value=\"" `TS.append` tok `TS.append` "\">")
                                       , ("csrf_meta", Func $ \_ -> lift $ do 
                                                                        tok <- getCSRFToken
                                                                        return $ LitText $ "<meta name=\"csrf-token\" content=\"" `TS.append` tok `TS.append` "\">")]
-- | Render a template or throw an error
renderTemplate :: (MonadIO m, StrappedApp g (WhebT g s m)) => String -> InputBucket (WhebT g s m) -> WhebHandlerT g s m
renderTemplate tName bucket = do
    sc <- getWithApp getStrappedContainer 
    result <- render (renderconfig sc) (combineBuckets bucket (defaultBucket sc)) tName
    case result of
        Left err -> throwError $ Error500 (T.pack $ show err)
        Right b -> builder "text/html" b
