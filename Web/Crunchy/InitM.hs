{-# LANGUAGE RecordWildCards #-}

module Web.Crunchy.InitM where
    
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           Network.Wai
import           Network.HTTP.Types.Method

import           Web.Crunchy.Internal
import           Web.Crunchy.Routes
import           Web.Crunchy.Types
import           Web.Crunchy.Utils

rGET :: (Maybe T.Text) -> UrlPat -> CrunchyHandler g s m -> Route g s m
rGET n p = Route n (==GET) (compilePat p)

rPOST :: (Maybe T.Text) -> UrlPat -> CrunchyHandler g s m -> Route g s m
rPOST n p = Route n (==POST) (compilePat p)

addGET :: T.Text -> UrlPat -> CrunchyHandler g s m -> InitM g s m ()
addGET n p h = addRoute $ rGET (Just n) p h

addPOST :: T.Text -> UrlPat -> CrunchyHandler g s m -> InitM g s m ()
addPOST n p h = addRoute $ rPOST (Just n) p h

addWAIMiddleware :: Middleware -> InitM g s m ()
addWAIMiddleware m = InitM $ tell $ mempty { initWaiMw = m }

addCrunchyMiddleware :: CrunchyMiddleware g s m -> InitM g s m ()
addCrunchyMiddleware m = InitM $ tell $ mempty { initCrunchyMw = [m] }

catchAllRoutes :: CrunchyHandler g s m -> InitM g s m ()
catchAllRoutes h = addRoute $ Route Nothing (const True) parser h
        where parser = UrlParser (const (Just [])) (const (Right $ T.pack "/*"))

addRoute :: Route g s m -> InitM g s m ()
addRoute r = addRoutes [r]

addRoutes :: [Route g s m] -> InitM g s m ()
addRoutes rs = InitM $ tell $ mempty { initRoutes = rs }

-- | Help prevent monomorphism errors for simple settings.
addSetting :: T.Text -> T.Text -> InitM g s m ()
addSetting = addSetting'

addSetting' :: Typeable a => T.Text -> a -> InitM g s m ()
addSetting' k v = addSettings $ M.fromList [(k, MkVal v)]

addSettings :: CSettings -> InitM g s m ()
addSettings settings = InitM $ tell $ mempty { initSettings = settings }


generateOptions :: MonadIO m => InitM g s m g -> IO (CrunchyOptions g s m)
generateOptions m = do 
  (g, InitOptions {..}) <- runWriterT (runInitM m)
  return $ CrunchyOptions { appRoutes = initRoutes
                         , runTimeSettings = initSettings
                         , port = 8080
                         , startingCtx = g
                         , waiStack = initWaiMw
                         , crunchyMiddlewares = initCrunchyMw
                         , defaultErrorHandler = defaultErr }