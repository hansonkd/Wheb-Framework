{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad.Error

import           Web.Wheb

import           Text.Strapped

data StrappedContainer m = StrappedContainer 
	{ renderconfig  :: RenderConfig 
	, defaultBucket :: InputBucket m
	}

class StrappedApp g where
	getStrappedContainer :: forall m . g -> (StrappedContainer m)

type MyApp = WhebT MyGlobalCtx () IO

data MyGlobalCtx = MyGlobalCtx (StrappedContainer MyApp)

instance StrappedApp MyGlobalCtx where
	getStrappedContainer (MyGlobalCtx g) = g

initStrapped :: MonadIO m => FilePath -> InitM g s m (StrappedContainer (WhebT g s m))
initStrapped fp = do
	mtmpls <- liftIO $ templateStoreFromDirectory fp ".html"
	case mtmpls of
		Left err -> error (show err)
		Right tmpls -> return $ StrappedContainer (defaultConfig { templateStore = tmpls }) (const Nothing)


renderTemplate :: (MonadIO m, StrappedApp g) => String -> InputBucket (WhebT g s m) -> WhebHandlerT g s m
renderTemplate tName bucket = do
	sc <- getWithApp getStrappedContainer
	result <- render (renderconfig sc) (combineBuckets bucket (defaultBucket sc)) tName
	case result of
		Left err -> throwError $ Error500 (show err)
		Right b -> builder "text/html" b


main :: IO ()
main = do
  opts <- generateOptions $ do
  	sc <- initStrapped "examples/resources"
  	addGET "." rootPat $ renderTemplate "index.html" (const Nothing)
  	return (MyGlobalCtx sc, ())
  runWhebServer opts