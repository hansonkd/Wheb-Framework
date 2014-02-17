{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.WhebT
  (
  -- * ReaderT and StateT Functionality
  -- ** ReaderT
    getApp
  , getWithApp
  -- ** StateT
  , getHandlerState
  , putHandlerState
  , modifyHandlerState
  , modifyHandlerState'
  
  -- * Responses
  , setHeader
  , setRawHeader
  , html
  , text
  , file
  , builder
  , renderTemplate
  , renderTemplate'
  
  -- * Settings
  , getSetting
  , getSetting'
  , getSetting''
  , getSettings
  
  -- * Routes
  , getRouteParams
  , getRouteParam
  , getRoute
  , getRoute'
  , getRawRoute
  
  -- * Request reading
  , getRequest
  , getRequestHeader
  , getWithRequest
  , getQueryParams
  , getPOSTParam
  , getPOSTParams
  , getRawPOST
  
  -- * Running Wheb
  , runWhebServer
  , runWhebServerT
  , debugHandler
  , debugHandlerT
  ) where

import           Blaze.ByteString.Builder (Builder)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception as E
import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (mk)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import           Data.Typeable (Typeable, cast)
import           Data.List (find)

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI
import           Network.Wai
import           Network.Wai.Handler.Warp as W
import           Network.Wai.Parse

import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import           Web.Wheb.Internal
import           Web.Wheb.Routes
import           Web.Wheb.Types
import           Web.Wheb.Utils

-- * ReaderT and StateT Functionality

-- ** ReaderT

-- | Get the 'g' in @WhebT g s m g@. This is a read-only state so only
-- thread-safe resources such as DB connections should go in here.
getApp :: Monad m => WhebT g s m g
getApp = WhebT $ liftM globalCtx ask

getWithApp :: Monad m => (g -> a) -> WhebT g s m a
getWithApp = flip liftM getApp

-- ** StateT

-- | Get the 's' in @WhebT g s m g@. This is a read and writable state
-- so you can get and put information in your state. Each request gets its own
-- fresh state duplicated from our options 'startingState'
getHandlerState :: Monad m => WhebT g s m s
getHandlerState = WhebT $ liftM reqState get

putHandlerState :: Monad m => s -> WhebT g s m ()
putHandlerState s = WhebT $ modify (\is -> is {reqState = s})

modifyHandlerState :: Monad m => (s -> s) -> WhebT g s m s
modifyHandlerState f = do
    s <- liftM f getHandlerState
    putHandlerState s
    return s

modifyHandlerState' :: Monad m => (s -> s) -> WhebT g s m ()
modifyHandlerState' f = modifyHandlerState f >> (return ())

-- * Settings

-- | Help prevent monomorphism errors for simple settings.
getSetting :: Monad m => T.Text -> WhebT g s m (Maybe T.Text)
getSetting = getSetting'

-- | Open up underlying support for polymorphic global settings
getSetting' :: (Monad m, Typeable a) => T.Text -> WhebT g s m (Maybe a)
getSetting' k = liftM (\cs -> (M.lookup k cs) >>= unwrap) getSettings
    where unwrap :: Typeable a => SettingsValue -> Maybe a
          unwrap (MkVal a) = cast a

-- | Get a setting or a default
getSetting'' :: (Monad m, Typeable a) => T.Text -> a -> WhebT g s m a
getSetting'' k d = liftM (fromMaybe d) (getSetting' k)

-- | Get all settings.
getSettings :: Monad m => WhebT g s m CSettings
getSettings = WhebT $ liftM (runTimeSettings . globalSettings) ask

-- * Routes

-- | Get all route params.
getRouteParams :: Monad m => WhebT g s m RouteParamList
getRouteParams = WhebT $ liftM routeParams ask

-- | Cast a route param into its type.
getRouteParam :: (Typeable a, Monad m) => T.Text -> WhebT g s m a
getRouteParam t = do
  p <- getRouteParam' t
  maybe (throwError RouteParamDoesNotExist) return p

-- | Cast a route param into its type.
getRouteParam' :: (Typeable a, Monad m) => T.Text -> WhebT g s m (Maybe a)
getRouteParam' t = liftM (getParam t) getRouteParams

-- | Convert 'Either' from 'getRoute'' into an error in the Monad
getRoute :: Monad m => T.Text -> RouteParamList ->  WhebT g s m T.Text
getRoute t l = do
        res <- getRoute' t l
        case res of
            Right t  -> return t
            Left err -> throwError $ URLError t err

-- | Generate a route from a name and param list.
getRoute' :: Monad m => T.Text -> 
             RouteParamList -> 
             WhebT g s m (Either UrlBuildError T.Text)
getRoute' n l = liftM buildRoute (getRawRoute n l)
    where buildRoute (Just (Route {..})) = generateUrl routeParser l
          buildRoute (Nothing)           = Left UrlNameNotFound

-- | Generate the raw route
getRawRoute :: Monad m => T.Text -> 
             RouteParamList -> 
             WhebT g s m (Maybe (Route g s m))
getRawRoute n l = WhebT $ liftM f ask  
    where findRoute (Route {..}) = fromMaybe False (fmap (==n) routeName)  
          f = ((find findRoute) . appRoutes . globalSettings)    

-- * Request reading

-- | Access the request
getRequest :: Monad m => WhebT g s m Request
getRequest = WhebT $ liftM request ask

getWithRequest :: Monad m => (Request -> a) -> WhebT g s m a
getWithRequest = flip liftM getRequest

-- | Get the raw parsed POST data including files.
getRawPOST :: MonadIO m => WhebT g s m ([Param], [File LBS.ByteString])
getRawPOST = WhebT $ liftM postData ask

-- | Get POST params as 'Text'
getPOSTParams :: MonadIO m => WhebT g s m [(T.Text, T.Text)]
getPOSTParams = liftM (fmap f . fst) getRawPOST
  where f (a, b) = (sbsToLazyText a, sbsToLazyText b)

-- | Maybe get one param if it exists.
getPOSTParam :: MonadIO m => T.Text -> WhebT g s m (Maybe T.Text)
getPOSTParam k = liftM (lookup k) getPOSTParams 

-- | Get params from URL (e.g. from '/foo/?q=4')
getQueryParams :: Monad m => WhebT g s m Query
getQueryParams = getWithRequest queryString

-- | Get a request header
getRequestHeader :: Monad m => T.Text -> WhebT g s m (Maybe T.Text)
getRequestHeader k = getRequest >>= f
  where hk = mk $ lazyTextToSBS k
        f = (return . (fmap sbsToLazyText) . (lookup hk) . requestHeaders)

-- * Responses

-- | Set a Strict ByteString header for the response
setRawHeader :: Monad m => Header -> WhebT g s m ()
setRawHeader (hn, hc) = WhebT $ modify insertHeader 
    where insertHeader is@(InternalState {..}) = 
            is { respHeaders = M.insert hn hc respHeaders }
 
-- | Set a header for the response
setHeader :: Monad m => T.Text -> T.Text -> WhebT g s m ()
setHeader hn hc = setRawHeader (mk $ lazyTextToSBS hn, lazyTextToSBS hc)

-- | Give filepath and content type to serve a file from disk.
file :: Monad m => T.Text -> T.Text -> WhebHandlerT g s m
file fp ct = do
    setHeader (T.pack "Content-Type") (ct) 
    return $ HandlerResponse status200 (WhebFile fp)

-- | Return simple HTML from Text
html :: Monad m => T.Text -> WhebHandlerT g s m
html c = do
    setHeader (T.pack "Content-Type") (T.pack "text/html") 
    return $ HandlerResponse status200 c

-- | Return simple Text 
text :: Monad m => T.Text -> WhebHandlerT g s m
text c = do
    setHeader (T.pack "Content-Type") (T.pack "text/plain") 
    return $ HandlerResponse status200 c

-- | Give content type and Blaze Builder
builder :: Monad m => T.Text -> Builder -> WhebHandlerT g s m
builder c b = do
    setHeader (T.pack "Content-Type") c 
    return $ HandlerResponse status200 b

-- | Render a template or throw a 500 error
renderTemplate :: MonadIO m => T.Text -> TemplateContext -> WhebHandlerT g s m
renderTemplate k ctx = renderTemplate' k ctx (T.pack "text/html")

-- | Like renderTemplate except designate content-type
renderTemplate' :: MonadIO m => T.Text -> 
                   TemplateContext -> 
                   T.Text -> 
                   WhebHandlerT g s m
renderTemplate' k ctx ct = do
  mTemplate <- WhebT $ liftM (M.lookup k . templates . globalSettings) ask
  case mTemplate of
    Nothing   -> throwError $ RenderError k TemplateNotFound
    Just (WhebTemplate func) -> do
      r <- liftIO (func ctx)
      either (throwError . RenderError k) (builder ct) r
      
-- * Running a Wheb Application

-- | Running a Handler with a custom Transformer
debugHandlerT :: WhebOptions g s m ->
             (m (Either WhebError a) -> IO (Either WhebError a)) ->
             Request ->
             WhebT g s m a ->
             IO (Either WhebError a)
debugHandlerT opts@(WhebOptions {..}) runIO r h = 
    runIO $ runDebugHandler opts h baseData
    where baseData = HandlerData startingCtx r ([], []) [] opts

-- | Convenience wrapper for 'debugHandlerT' function in 'IO'
debugHandler :: WhebOptions g s IO -> 
              WhebT g s IO a ->
              IO (Either WhebError a)
debugHandler opts h = debugHandlerT opts id defaultRequest h

-- | Run a server with a function to run your inner Transformer to IO and 
-- generated options
runWhebServerT :: (m EResponse -> IO EResponse) ->
                  WhebOptions g s m ->
                  IO ()
runWhebServerT runIO opts@(WhebOptions {..}) = do
    putStrLn $ "Now running on port " ++ (show $ port)

    installHandler sigINT catchSig Nothing
    installHandler sigTERM catchSig Nothing

    forkIO $ runSettings rtSettings $
        gracefulExit $
        waiStack $ 
        optsToApplication opts runIO

    loop
    waitForConnections
    putStrLn $ "Shutting down server..."
    sequence_ cleanupActions

  where catchSig = (Catch (atomically $ writeTVar shutdownTVar True))
        loop = do
          shutDown <- atomically $ readTVar shutdownTVar
          if shutDown then return () else (threadDelay 100000) >> loop
        gracefulExit app r = do
          isExit <- atomically $ readTVar shutdownTVar
          case isExit of
              False -> app r
              True  -> return $ responseLBS serviceUnavailable503 [] LBS.empty
        waitForConnections = do
          openConnections <- atomically $ readTVar activeConnections
          if (openConnections > 0)
            then waitForConnections
            else return ()
        port = fromMaybe 3000 $ 
          (M.lookup (T.pack "port") runTimeSettings) >>= (\(MkVal m) -> cast m)
        rtSettings = warpSettings { W.settingsPort = port }

-- | Convenience wrapper for 'runWhebServerT' function in IO
runWhebServer :: (WhebOptions g s IO) -> IO ()
runWhebServer = runWhebServerT id
