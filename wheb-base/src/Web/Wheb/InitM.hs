{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.InitM
  (
  -- * Routes
  -- ** Named routes convenience functions
    addGET
  , addPOST
  , addPUT
  , addDELETE
  -- ** Sites
  , addSite
  -- ** Add raw routes
  , addRoute
  , addRoutes
  , catchAll
  -- ** Sockets
  , addWhebSocket
  -- * Middlewares
  , addWAIMiddleware
  , addWhebMiddleware
  -- * Settings
  , addSetting
  , addSetting'
  , addSettings
  , readSettingsFile
  -- * Cleanup
  , addCleanupHook
  -- * Running
  , generateOptions
  , genMinOpts
  ) where

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Writer (liftM, MonadWriter(tell), Monoid(mempty), WriterT(runWriterT))
import qualified Data.Map as M (empty, fromList)
import qualified Data.Text as T (lines, pack, splitOn, strip, Text, unpack)
import qualified Data.Text.IO as T (readFile)
import Data.Typeable (Typeable)
import Network.HTTP.Types.Method (StdMethod(DELETE, GET, POST, PUT))
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, setOnClose, setOnOpen)
import Text.Read (readMaybe)
import Web.Routes (Site(..))
import Web.Wheb.Routes (patRoute, compilePat)
import Web.Wheb.Types (CSettings, InitM(..), InitOptions(..), 
                       InternalState(InternalState), MinOpts, PackedSite(PackedSite), Route(Route), SettingsValue(MkVal), UrlParser(UrlParser), 
                       UrlPat, WhebHandlerT, WhebMiddleware, WhebOptions(..), SocketRoute(SocketRoute), WhebSocket)
import Web.Wheb.Utils (defaultErr)

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

addSite :: T.Text -> Site url (WhebHandlerT g s m) -> InitM g s m ()
addSite t s = InitM $ tell $ mempty { initSites = [PackedSite t s] }

addWhebSocket :: UrlPat -> WhebSocket g s m -> InitM g s m ()
addWhebSocket p h = InitM $ tell $ mempty { initWhebSockets = [SocketRoute (compilePat p) h] }

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
  let set1 = setOnOpen (\_ -> atomically (addToTVar ac) >> return True) defaultSettings 
      warpsettings = setOnClose (\_ -> atomically (subFromTVar ac)) set1
  return $ WhebOptions { appRoutes = initRoutes
                       , appWhebSockets = initWhebSockets
                       , appSites  = initSites 
                       , runTimeSettings = initSettings
                       , warpSettings = warpsettings
                       , startingCtx = g
                       , startingState = InternalState s M.empty []
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
