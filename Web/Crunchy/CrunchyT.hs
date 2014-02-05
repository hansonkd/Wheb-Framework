{-# LANGUAGE RecordWildCards #-}

module Web.Crunchy.CrunchyT where

import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (mk)
import           Data.Default
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import           Data.Typeable (Typeable, cast)
import           Data.List (find)

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse

import           Web.Crunchy.Internal
import           Web.Crunchy.Routes
import           Web.Crunchy.Types
import           Web.Crunchy.Utils

getApp :: Monad m => CrunchyT g s m g
getApp = CrunchyT $ liftM globalCtx ask

getWithApp :: Monad m => (g -> a) -> CrunchyT g s m a
getWithApp = flip liftM getApp

getReqState :: Monad m => CrunchyT g s m s
getReqState = CrunchyT $ liftM reqState get

putReqState :: Monad m => s -> CrunchyT g s m ()
putReqState s = CrunchyT $ modify (\is -> is {reqState = s})

modifyReqState :: Monad m => (s -> s) -> CrunchyT g s m s
modifyReqState f = do
    s <- liftM f getReqState
    putReqState s
    return s

modifyReqState' :: Monad m => (s -> s) -> CrunchyT g s m ()
modifyReqState' f = modifyReqState f >> (return ())

-- | Help prevent monomorphism errors for simple settings.
getSetting :: Monad m => T.Text -> CrunchyT g s m (Maybe T.Text)
getSetting = getSetting'

-- | Open up underlying support for Polymorphic global settings
getSetting' :: (Monad m, Typeable a) => T.Text -> CrunchyT g s m (Maybe a)
getSetting' k = liftM (\cs -> (M.lookup k cs) >>= unwrap) getSettings
    where unwrap :: Typeable a => SettingsValue -> Maybe a
          unwrap (MkVal a) = cast a

getSettings :: Monad m => CrunchyT g s m CSettings
getSettings = CrunchyT $ liftM (runTimeSettings . globalSettings) ask

getRouteParams :: Monad m => CrunchyT g s m RouteParamList
getRouteParams = CrunchyT $ liftM routeParams ask

getRouteParam :: (Typeable a, Monad m) => T.Text -> CrunchyT g s m (Maybe a)
getRouteParam t = liftM (getParam t) getRouteParams

getRoute' :: Monad m => T.Text -> 
             RouteParamList -> 
             CrunchyT g s m (Either UrlBuildError T.Text)
getRoute' n l = CrunchyT $ liftM f ask
    where findRoute (Route {..}) = fromMaybe False (fmap (==n) routeName)
          buildRoute (Just (Route {..})) = generateUrl routeParser l
          buildRoute (Nothing)           = Left UrlNameNotFound
          f = (buildRoute . (find findRoute) . appRoutes . globalSettings)

getRoute :: Monad m => T.Text -> RouteParamList ->  CrunchyT g s m T.Text
getRoute t l = do
        res <- getRoute' t l
        case res of
            Right t  -> return t
            Left err -> throwError $ URLError t err

getRequest :: Monad m => CrunchyT g s m Request
getRequest = CrunchyT $ liftM request ask

getWithRequest :: Monad m => (Request -> a) -> CrunchyT g s m a
getWithRequest = flip liftM getRequest

getRawPOST :: MonadIO m => CrunchyT g s m ([Param], [File LBS.ByteString])
getRawPOST = CrunchyT $ liftM postData ask

getPOSTParams :: MonadIO m => CrunchyT g s m [(T.Text, T.Text)]
getPOSTParams = liftM (fmap f . fst) getRawPOST
  where f (a, b) = (sbsToLazyText a, sbsToLazyText b)

getPostParam :: MonadIO m => T.Text -> CrunchyT g s m (Maybe T.Text)
getPostParam k = liftM (lookup k) getPOSTParams 

getQueryParams :: Monad m => CrunchyT g s m Query
getQueryParams = getWithRequest queryString

getHeader :: Monad m => T.Text -> CrunchyT g s m (Maybe T.Text)
getHeader k = getRequest >>= f
  where hk = mk $ lazyTextToSBS k
        f = (return . (fmap sbsToLazyText) . (lookup hk) . requestHeaders)

setRawHeader :: Monad m => Header -> CrunchyT g s m ()
setRawHeader (hn, hc) = CrunchyT $ modify insertHeader 
    where insertHeader is@(InternalState {..}) = 
            is { respHeaders = M.insert hn hc respHeaders }

setHeader :: Monad m => T.Text -> T.Text -> CrunchyT g s m ()
setHeader hn hc = setRawHeader (mk $ lazyTextToSBS hn, lazyTextToSBS hc)

html :: Monad m => T.Text -> CrunchyHandler g s m
html c = do
    setHeader (T.pack "Content-Type") (T.pack "text/html") 
    return $ HandlerResponse status200 c
    
text :: Monad m => T.Text -> CrunchyHandler g s m
text c = do
    setHeader (T.pack "Content-Type") (T.pack "text/plain") 
    return $ HandlerResponse status200 c

----------------------- Running the application -----------------------
debugHandlerT :: (Default s) => CrunchyOptions g s m ->
             (m (Either CrunchyError a) -> IO (Either CrunchyError a)) ->
             Request ->
             CrunchyT g s m a ->
             IO (Either CrunchyError a)
debugHandlerT opts@(CrunchyOptions {..}) runIO r h = 
    runIO $ runDebugHandler opts h baseData
    where baseData = HandlerData startingCtx r ([], []) [] opts

debugHandlerIO :: (Default s) => CrunchyOptions g s IO -> 
              CrunchyT g s IO a ->
              IO (Either CrunchyError a)
debugHandlerIO opts h = debugHandlerT opts id defaultRequest h

runCrunchyServerT :: (Default s) => 
                  (m EResponse -> IO EResponse) ->
                  CrunchyOptions g s m ->
                  IO ()
runCrunchyServerT runIO opts = do
    putStrLn $ "Now running on port " ++ (show $ port opts)
    run (port opts) $ 
        (waiStack opts) $ 
        optsToApplication opts runIO

runCrunchyServer :: (Default s) => 
                 (CrunchyOptions g s IO) ->
                 IO ()
runCrunchyServer = runCrunchyServerT id
