{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Wheb.Plugins.Strapped
  ( StrappedContainer(..)
  , StrappedApp (..)
  , renderTemplate
  , initStrapped    
  , module Text.Strapped
  ) where



import           Control.Monad.Except
import qualified Data.Text.Lazy as T
import           Web.Wheb

import           Text.Strapped

data StrappedContainer m = StrappedContainer 
    { renderconfig  :: RenderConfig 
    , defaultBucket :: InputBucket m
    }

class StrappedApp g m where
    getStrappedContainer :: g -> StrappedContainer m

-- | Load Strapped from a directory matching the extention
initStrapped :: MonadIO m => FilePath -> String -> InitM g s m (StrappedContainer (WhebT g s m))
initStrapped fp s = do
    mtmpls <- liftIO $ templateStoreFromDirectory fp s
    case mtmpls of
        Left err -> error (show err)
        Right tmpls -> return $ StrappedContainer (defaultConfig { templateStore = tmpls }) []

-- | Render a template or throw an error
renderTemplate :: (MonadIO m, StrappedApp g (WhebT g s m)) => String -> InputBucket (WhebT g s m) -> WhebHandlerT g s m
renderTemplate tName bucket = do
    sc <- getWithApp getStrappedContainer 
    result <- render (renderconfig sc) (combineBuckets bucket (defaultBucket sc)) tName
    case result of
        Left err -> throwError $ Error500 (T.pack $ show err)
        Right b -> builder "text/html" b
