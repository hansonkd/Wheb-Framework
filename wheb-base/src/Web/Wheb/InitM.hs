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
  , catchAll
  -- * Middlewares
  , addWAIMiddleware
  , addWhebMiddleware
  -- * Settings
  , addSetting
  , addSetting'
  , addSettings
  , readSettingsFile
  -- * Templates
  , addTemplate
  , addTemplates
  -- * Cleanup
  , addCleanupHook
  -- * Running
  , generateOptions
  , genMinOpts
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Char (isSpace)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Typeable
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultSettings
                                          , settingsOnOpen
                                          , settingsOnClose)
import           Network.HTTP.Types.Method
import           Text.Read (readMaybe)

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

-- | Catch all requests regardless of method or path
catchAll :: WhebHandlerT g s m -> InitM g s m ()
catchAll h = addRoute $ Route Nothing (const True) parser h
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

addTemplate :: T.Text -> WhebTemplate -> InitM g s m ()
addTemplate k tmpl = addTemplates $ M.fromList [(k, tmpl)]

addTemplates :: TemplateMap -> InitM g s m ()
addTemplates tmpls = InitM $ tell $ mempty { initTemplates = tmpls }

-- | Reads a file line by line and splits keys and values by \":\".
--   Uses default "Text.Read" to try to match 'Int', 'Bool' or 'Float' and will add
--   specific typed settings for those.
readSettingsFile :: FilePath -> InitM g s m ()
readSettingsFile fp = (liftIO $ liftM T.lines (T.readFile fp)) >>= (mapM_ parseLines)
  where parseLines line = 
            case T.splitOn (T.pack ":") line of 
                a:b:_ -> do
                    let k = T.strip a
                        v = T.strip b
                    maybePutSetting k v (readText :: (T.Text -> Maybe Int))
                    maybePutSetting k v (readText :: (T.Text -> Maybe Bool))
                    maybePutSetting k v (readText :: (T.Text -> Maybe Float))
                    addSetting k v
                _     -> return ()
        readText :: Read a => T.Text -> Maybe a
        readText = readMaybe . T.unpack
        maybePutSetting k t parse = maybe (return ()) (addSetting' k) (parse t)

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
                       , cleanupActions = initCleanup
                       , templates = initTemplates }
  where addToTVar ac = ((readTVar ac) >>= (\cs -> writeTVar ac (succ cs)))
        subFromTVar ac = ((readTVar ac) >>= (\cs -> writeTVar ac (pred cs)))

-- | Generate options for an application without a context or state
genMinOpts :: InitM () () IO () -> IO MinOpts
genMinOpts m = generateOptions (m >> (return ((), ()))) 