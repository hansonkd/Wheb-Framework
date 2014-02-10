{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.InitM
  (
  -- * Routes
  -- ** Named routes convenience functions
    addGET
  , addPOST
  , addPUT
  , addDELETE
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
  -- * Cleanup
  , addCleanupHook
  -- * Running
  , generateOptions
  , genMinOpts
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultSettings
                                          , settingsOnOpen
                                          , settingsOnClose)
import           Network.HTTP.Types.Method

import           Web.Wheb.Internal
import           Web.Wheb.Routes
import           Web.Wheb.Types
import           Web.Wheb.Utils

addGET :: T.Text -> UrlPat -> WhebHandlerT g s m -> InitM g s m ()
addGET n p h = addRoute $ patRoute (Just n) GET p h

addPOST :: T.Text -> UrlPat -> WhebHandlerT g s m -> InitM g s m ()
addPOST n p h = addRoute $ patRoute (Just n) POST p h

addPUT :: T.Text -> UrlPat -> WhebHandlerT g s m -> InitM g s m ()
addPUT n p h = addRoute $ patRoute (Just n) PUT p h

addDELETE :: T.Text -> UrlPat -> WhebHandlerT g s m -> InitM g s m ()
addDELETE n p h = addRoute $ patRoute (Just n) DELETE p h

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

-- | IO Actions to run after server has been stopped.
addCleanupHook :: IO () -> InitM g s m ()
addCleanupHook action = InitM $ tell $ mempty { initCleanup = [action] }

-- | Generate 'WhebOptions' from 'InitM' in 'IO'
generateOptions :: MonadIO m => InitM g s m (g, s) -> IO (WhebOptions g s m)
generateOptions m = do 
  ((g, s), InitOptions {..}) <- runWriterT (runInitM m)
  tv <- liftIO $ newTVarIO False
  ac <- liftIO $ newTVarIO 0
  let warpsettings = defaultSettings 
                        { settingsOnOpen = atomically (addToTVar ac)
                        , settingsOnClose = atomically (subFromTVar ac)}
  return $ WhebOptions { appRoutes = initRoutes
                         , runTimeSettings = initSettings
                         , warpSettings = warpsettings
                         , startingCtx = g
                         , startingState = InternalState s M.empty
                         , waiStack = initWaiMw
                         , whebMiddlewares = initWhebMw
                         , defaultErrorHandler = defaultErr
                         , shutdownTVar  = tv
                         , activeConnections = ac
                         , cleanupActions = initCleanup }
  where addToTVar ac = ((readTVar ac) >>= (\cs -> writeTVar ac (succ cs)))
        subFromTVar ac = ((readTVar ac) >>= (\cs -> writeTVar ac (pred cs)))

-- | Generate options for an application without a context or state
genMinOpts :: InitM () () IO () -> IO MinOpts
genMinOpts m = generateOptions (m >> (return ((), ()))) 