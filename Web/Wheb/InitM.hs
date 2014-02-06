{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.InitM
  (
  -- * Routes
  -- ** Named routes convenience functions
    addGET
  , addPOST
  -- ** Add raw routes
  , addRoute
  , addRoutes
  -- * Middlewares
  , addWAIMiddleware
  , addWhebMiddleware
  -- * Settings
  , addSetting
  , addSetting'
  , addSettings
  
  -- * Running
  , generateOptions
  
  ) where
    
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultSettings)
import           Network.HTTP.Types.Method

import           Web.Wheb.Internal
import           Web.Wheb.Routes
import           Web.Wheb.Types
import           Web.Wheb.Utils

addGET :: T.Text -> UrlPat -> WhebHandlerT g s m -> InitM g s m ()
addGET n p h = addRoute $ rGET (Just n) p h

addPOST :: T.Text -> UrlPat -> WhebHandlerT g s m -> InitM g s m ()
addPOST n p h = addRoute $ rPOST (Just n) p h

addRoute :: Route g s m -> InitM g s m ()
addRoute r = addRoutes [r]

addRoutes :: [Route g s m] -> InitM g s m ()
addRoutes rs = InitM $ tell $ mempty { initRoutes = rs }

-- | Catch all routes regardless of method or path
catchAllRoutes :: WhebHandlerT g s m -> InitM g s m ()
catchAllRoutes h = addRoute $ Route Nothing (const True) parser h
        where parser = UrlParser (const (Just [])) (const (Right $ T.pack "/*"))

-- | Add generic "WAI" middleware
addWAIMiddleware :: Middleware -> InitM g s m ()
addWAIMiddleware m = InitM $ tell $ mempty { initWaiMw = m }

-- | Add "Wheb" specific middleware
addWhebMiddleware :: WhebMiddleware g s m -> InitM g s m ()
addWhebMiddleware m = InitM $ tell $ mempty { initWhebMw = [m] }


-- | Wrapped 'addSetting'' to help prevent monomorphism errors for simple settings.
addSetting :: T.Text -> T.Text -> InitM g s m ()
addSetting = addSetting'

-- | Adds a setting value, replacing it if its key already exists.
addSetting' :: Typeable a => T.Text -> a -> InitM g s m ()
addSetting' k v = addSettings $ M.fromList [(k, MkVal v)]

addSettings :: CSettings -> InitM g s m ()
addSettings settings = InitM $ tell $ mempty { initSettings = settings }

-- | Generate 'WhebOptions' from 'InitM' in 'IO'
generateOptions :: MonadIO m => InitM g s m g -> IO (WhebOptions g s m)
generateOptions m = do 
  (g, InitOptions {..}) <- runWriterT (runInitM m)
  return $ WhebOptions { appRoutes = initRoutes
                         , runTimeSettings = initSettings
                         , warpSettings = defaultSettings
                         , startingCtx = g
                         , waiStack = initWaiMw
                         , whebMiddlewares = initWhebMw
                         , defaultErrorHandler = defaultErr }