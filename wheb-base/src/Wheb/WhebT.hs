{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Wheb.WhebT
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
  , redirect
  , throwRedirect
  
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
  , runRawHandler
  , runRawHandler'
  ) where

import           Blaze.ByteString.Builder (Builder)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (atomically, readTVar, 
                                         newTVarIO, writeTVar)
import           Control.Monad (void)
import           Control.Monad.Except (liftM, MonadError(throwError), MonadIO)
import           Control.Monad.Reader (MonadReader(ask))
import           Control.Monad.State.Strict (modify', MonadState(get))
import qualified Data.ByteString.Lazy as LBS (ByteString, empty)
import           Data.CaseInsensitive (mk)
import           Data.List (find)
import           Data.String (fromString)
import qualified Data.Map as M (insert, lookup)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import           Data.Typeable (cast, Typeable)
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Status (serviceUnavailable503, status200, status302)
import           Network.HTTP.Types.URI (Query)
import           Network.Wai (defaultRequest, Request(queryString, requestHeaders), responseLBS)
import           Network.Wai.Handler.Warp as W (runSettings, setPort, setHost)
import           Network.Wai.Parse (File, Param)
import           System.Posix.Signals (Handler(Catch), installHandler, sigINT, sigTERM)
import           Wheb.Internal (optsToApplication, runDebugHandler)
import           Wheb.Routes (generateUrl, getParam)
import           Wheb.Types

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
putHandlerState s = WhebT $ modify' (\is -> is {reqState = s})

modifyHandlerState :: Monad m => (s -> s) -> WhebT g s m s
modifyHandlerState f = do
    s <- liftM f getHandlerState
    putHandlerState s
    return s

modifyHandlerState' :: Monad m => (s -> s) -> WhebT g s m ()
modifyHandlerState' f = modifyHandlerState f >> (return ())

-- * Settings

-- | Help prevent monomorphism errors for simple settings.
getSetting :: Monad m => TS.Text -> WhebT g s m (Maybe T.Text)
getSetting = getSetting'

-- | Open up underlying support for polymorphic global settings
getSetting' :: (Monad m, Typeable a) => TS.Text -> WhebT g s m (Maybe a)
getSetting' k = liftM (\cs -> (M.lookup k cs) >>= unwrap) getSettings
    where unwrap :: Typeable a => SettingsValue -> Maybe a
          unwrap (MkVal a) = cast a

-- | Get a setting or a default
getSetting'' :: (Monad m, Typeable a) => TS.Text -> a -> WhebT g s m a
getSetting'' k d = liftM (fromMaybe d) (getSetting' k)

-- | Get all settings.
getSettings :: Monad m => WhebT g s m CSettings
getSettings = WhebT $ liftM (handlerRunTimeSettings) ask

-- * Routes

-- | Get all route params.
getRouteParams :: Monad m => WhebT g s m RouteParamList
getRouteParams = WhebT $ liftM routeParams ask

-- | Cast a route param into its type.
getRouteParam :: (Typeable a, Monad m) => TS.Text -> WhebT g s m a
getRouteParam t = do
  p <- getRouteParam' t
  maybe (throwError RouteParamDoesNotExist) return p

-- | Cast a route param into its type.
getRouteParam' :: (Typeable a, Monad m) => TS.Text -> WhebT g s m (Maybe a)
getRouteParam' t = liftM (getParam t) getRouteParams

-- | Convert 'Either' from 'getRoute'' into an error in the Monad
getRoute :: Monad m => TS.Text -> RouteParamList ->  WhebT g s m TS.Text
getRoute name l = do
        res <- getRoute' name l
        case res of
            Right t  -> return t
            Left err -> throwError $ URLError name err

-- | Generate a route from a name and param list.
getRoute' :: Monad m => TS.Text -> 
             RouteParamList -> 
             WhebT g s m (Either UrlBuildError TS.Text)
getRoute' n l = liftM buildRoute (getRawRoute n l)
    where buildRoute (Just (Route {..})) = generateUrl routeParser l
          buildRoute (Nothing)           = Left UrlNameNotFound

-- | Generate the raw route
getRawRoute :: Monad m => TS.Text -> 
             RouteParamList -> 
             WhebT g s m (Maybe (Route g s m))
getRawRoute n _ = WhebT $ liftM f ask  
    where findRoute (Route {..}) = fromMaybe False (fmap (==n) routeName)  
          f = ((find findRoute) . handlerAppRoutes)    

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
getPOSTParams :: MonadIO m => WhebT g s m [(TS.Text, TS.Text)]
getPOSTParams = liftM (fmap f . fst) getRawPOST
  where f (a, b) = (TS.decodeUtf8 a, TS.decodeUtf8 b)

-- | Maybe get one param if it exists.
getPOSTParam :: MonadIO m => TS.Text -> WhebT g s m (Maybe TS.Text)
getPOSTParam k = liftM (lookup k) getPOSTParams 

-- | Get params from URL (e.g. from '/foo/?q=4')
getQueryParams :: Monad m => WhebT g s m Query
getQueryParams = getWithRequest queryString

-- | Get a request header
getRequestHeader :: Monad m => TS.Text -> WhebT g s m (Maybe TS.Text)
getRequestHeader k = getRequest >>= f
  where hk = mk $ TS.encodeUtf8 k
        f = (return . (fmap TS.decodeUtf8) . (lookup hk) . requestHeaders)

-- * Responses

-- | Set a Strict ByteString header for the response
setRawHeader :: Monad m => Header -> WhebT g s m ()
setRawHeader (hn, hc) = WhebT $ modify' insertHeader 
    where insertHeader is@(InternalState {..}) = 
            is { respHeaders = M.insert hn hc respHeaders }
 
-- | Set a header for the response
setHeader :: Monad m => TS.Text -> TS.Text -> WhebT g s m ()
setHeader hn hc = setRawHeader (mk $ TS.encodeUtf8 hn, TS.encodeUtf8 hc)

-- | Give filepath and content-type to serve a file via lazy text.
file :: Monad m => TS.Text -> TS.Text -> WhebHandlerT g s m
file fp ct = do
    setHeader (TS.pack "Content-Type") (ct) 
    return $ HandlerResponse status200 (WhebFile fp)

-- | Return simple HTML from lazy Text
html :: Monad m => T.Text -> WhebHandlerT g s m
html c = do
    setHeader (TS.pack "Content-Type") (TS.pack "text/html") 
    return $ HandlerResponse status200 c

-- | Return simple lazy Text 
text :: Monad m => T.Text -> WhebHandlerT g s m
text c = do
    setHeader (TS.pack "Content-Type") (TS.pack "text/plain") 
    return $ HandlerResponse status200 c

-- | Give content-type and Blaze Builder
builder :: Monad m => TS.Text -> Builder -> WhebHandlerT g s m
builder c b = do
    setHeader (TS.pack "Content-Type") c 
    return $ HandlerResponse status200 b

-- | Redirect to a given URL
redirect :: Monad m => TS.Text -> WhebHandlerT g s m
redirect c = do
    setHeader (TS.pack "Location") c
    return $ HandlerResponse status302 T.empty
    
-- | Thow a redirect as an error
throwRedirect :: Monad m => TS.Text -> WhebHandlerT g s m
throwRedirect c = do
    setHeader (TS.pack "Location") c
    throwError $ ErrorStatus status302 T.empty
    
-- * Running a Wheb Application

-- | Running a Handler with a custom Transformer
runRawHandler' :: WhebOptions g s m ->
             Request ->
             WhebT g s m a ->
             IO (Either WhebError a)
runRawHandler' opts@(WhebOptions {..}) r h = 
    runToIO $ runDebugHandler opts h baseData
    where baseData = HandlerData startingCtx r ([], []) [] runTimeSettings appRoutes

-- | Convenience wrapper with 'defaultRequest' for 'runRawHandler''
runRawHandler :: WhebOptions g s m -> 
              WhebT g s m a ->
              IO (Either WhebError a)
runRawHandler opts h = runRawHandler' opts defaultRequest h

-- | Run a server with a function to run your inner Transformer to IO and 
-- generated options
runWhebServer :: String -> Int ->  WhebOptions g s m -> IO ()
runWhebServer host port opts@(WhebOptions {..}) = do
    putStrLn $ "Now running on port " ++ (show $ port)

    forceTVar <- newTVarIO False

    void $ installHandler sigINT catchSig Nothing
    void $ installHandler sigTERM catchSig Nothing

    void $ forkIO $ runSettings rtSettings $
            gracefulExit $
            waiStack $
            optsToApplication opts

    let termSig = (Catch (atomically $ writeTVar forceTVar True >> writeTVar shutdownTVar True))
        installForceKill = installHandler sigTERM termSig Nothing >> installHandler sigINT termSig Nothing

    void $ loop installForceKill
    putStrLn $ "Waiting for connections to close..."
    
    waitForConnections forceTVar
    putStrLn $ "Shutting down server..."
    
    sequence_ cleanupActions

  where catchSig = (Catch (atomically $ writeTVar shutdownTVar True))
        loop terminate = do
          shutDown <- atomically $ readTVar shutdownTVar
          if shutDown then terminate else (threadDelay 100000) >> loop terminate
        gracefulExit app r respond = do
          isExit <- atomically $ readTVar shutdownTVar
          case isExit of
              False -> app r respond
              True  -> respond $ responseLBS serviceUnavailable503 [] LBS.empty
        waitForConnections forceTVar = do
          openConnections <- atomically $ readTVar activeConnections
          force <- atomically $ readTVar forceTVar
          if (openConnections == 0 || force)
            then return ()
            else waitForConnections forceTVar
        rtSettings = W.setHost (fromString host) $ W.setPort port warpSettings
