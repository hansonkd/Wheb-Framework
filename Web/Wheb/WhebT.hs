{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.WhebT where

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

import           Web.Wheb.Internal
import           Web.Wheb.Routes
import           Web.Wheb.Types
import           Web.Wheb.Utils

getApp :: Monad m => WhebT g s m g
getApp = WhebT $ liftM globalCtx ask

getWithApp :: Monad m => (g -> a) -> WhebT g s m a
getWithApp = flip liftM getApp

getReqState :: Monad m => WhebT g s m s
getReqState = WhebT $ liftM reqState get

putReqState :: Monad m => s -> WhebT g s m ()
putReqState s = WhebT $ modify (\is -> is {reqState = s})

modifyReqState :: Monad m => (s -> s) -> WhebT g s m s
modifyReqState f = do
    s <- liftM f getReqState
    putReqState s
    return s

modifyReqState' :: Monad m => (s -> s) -> WhebT g s m ()
modifyReqState' f = modifyReqState f >> (return ())

-- | Help prevent monomorphism errors for simple settings.
getSetting :: Monad m => T.Text -> WhebT g s m (Maybe T.Text)
getSetting = getSetting'

-- | Open up underlying support for Polymorphic global settings
getSetting' :: (Monad m, Typeable a) => T.Text -> WhebT g s m (Maybe a)
getSetting' k = liftM (\cs -> (M.lookup k cs) >>= unwrap) getSettings
    where unwrap :: Typeable a => SettingsValue -> Maybe a
          unwrap (MkVal a) = cast a

getSettings :: Monad m => WhebT g s m CSettings
getSettings = WhebT $ liftM (runTimeSettings . globalSettings) ask

getRouteParams :: Monad m => WhebT g s m RouteParamList
getRouteParams = WhebT $ liftM routeParams ask

getRouteParam :: (Typeable a, Monad m) => T.Text -> WhebT g s m (Maybe a)
getRouteParam t = liftM (getParam t) getRouteParams

getRoute' :: Monad m => T.Text -> 
             RouteParamList -> 
             WhebT g s m (Either UrlBuildError T.Text)
getRoute' n l = WhebT $ liftM f ask
    where findRoute (Route {..}) = fromMaybe False (fmap (==n) routeName)
          buildRoute (Just (Route {..})) = generateUrl routeParser l
          buildRoute (Nothing)           = Left UrlNameNotFound
          f = (buildRoute . (find findRoute) . appRoutes . globalSettings)

getRoute :: Monad m => T.Text -> RouteParamList ->  WhebT g s m T.Text
getRoute t l = do
        res <- getRoute' t l
        case res of
            Right t  -> return t
            Left err -> throwError $ URLError t err

getRequest :: Monad m => WhebT g s m Request
getRequest = WhebT $ liftM request ask

getWithRequest :: Monad m => (Request -> a) -> WhebT g s m a
getWithRequest = flip liftM getRequest

getRawPOST :: MonadIO m => WhebT g s m ([Param], [File LBS.ByteString])
getRawPOST = WhebT $ liftM postData ask

getPOSTParams :: MonadIO m => WhebT g s m [(T.Text, T.Text)]
getPOSTParams = liftM (fmap f . fst) getRawPOST
  where f (a, b) = (sbsToLazyText a, sbsToLazyText b)

getPostParam :: MonadIO m => T.Text -> WhebT g s m (Maybe T.Text)
getPostParam k = liftM (lookup k) getPOSTParams 

getQueryParams :: Monad m => WhebT g s m Query
getQueryParams = getWithRequest queryString

getHeader :: Monad m => T.Text -> WhebT g s m (Maybe T.Text)
getHeader k = getRequest >>= f
  where hk = mk $ lazyTextToSBS k
        f = (return . (fmap sbsToLazyText) . (lookup hk) . requestHeaders)

setRawHeader :: Monad m => Header -> WhebT g s m ()
setRawHeader (hn, hc) = WhebT $ modify insertHeader 
    where insertHeader is@(InternalState {..}) = 
            is { respHeaders = M.insert hn hc respHeaders }

setHeader :: Monad m => T.Text -> T.Text -> WhebT g s m ()
setHeader hn hc = setRawHeader (mk $ lazyTextToSBS hn, lazyTextToSBS hc)

html :: Monad m => T.Text -> WhebHandlerT g s m
html c = do
    setHeader (T.pack "Content-Type") (T.pack "text/html") 
    return $ HandlerResponse status200 c
    
text :: Monad m => T.Text -> WhebHandlerT g s m
text c = do
    setHeader (T.pack "Content-Type") (T.pack "text/plain") 
    return $ HandlerResponse status200 c

----------------------- Running the application -----------------------
debugHandlerT :: (Default s) => WhebOptions g s m ->
             (m (Either WhebError a) -> IO (Either WhebError a)) ->
             Request ->
             WhebT g s m a ->
             IO (Either WhebError a)
debugHandlerT opts@(WhebOptions {..}) runIO r h = 
    runIO $ runDebugHandler opts h baseData
    where baseData = HandlerData startingCtx r ([], []) [] opts

debugHandlerIO :: (Default s) => WhebOptions g s IO -> 
              WhebT g s IO a ->
              IO (Either WhebError a)
debugHandlerIO opts h = debugHandlerT opts id defaultRequest h

runWhebServerT :: (Default s) => 
                  (m EResponse -> IO EResponse) ->
                  WhebOptions g s m ->
                  IO ()
runWhebServerT runIO opts = do
    putStrLn $ "Now running on port " ++ (show $ port opts)
    run (port opts) $ 
        (waiStack opts) $ 
        optsToApplication opts runIO

runWhebServer :: (Default s) => 
                 (WhebOptions g s IO) ->
                 IO ()
runWhebServer = runWhebServerT id
