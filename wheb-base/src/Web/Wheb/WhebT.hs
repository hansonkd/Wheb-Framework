{-# LANGUAGE RecordWildCards, RankNTypes #-}

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
  , runWhebServerT
  , runRawHandler
  , runRawHandlerT
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, readTVar, newTVarIO, writeTVar)
import Control.Monad.Error (liftM, MonadError(throwError), MonadIO, void)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.State (modify, MonadState(get))
import qualified Data.ByteString.Lazy as LBS (ByteString, empty)
import Data.CaseInsensitive (mk)
import Data.List (find)
import qualified Data.Map as M (insert, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as TS (pack, empty, Text)
import qualified Data.Text.Encoding as TS (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T (pack, empty, Text)
import Data.Typeable (cast, Typeable)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (serviceUnavailable503, status200, status302)
import Network.HTTP.Types.URI (Query)
import Network.Wai (defaultRequest, Request(queryString, requestHeaders), responseLBS)
import Network.Wai.Handler.Warp as W (runSettings, setPort)
import Network.Wai.Parse (File, Param)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT, sigTSTP, sigTERM)
import Web.Wheb.Internal (optsToApplication, runDebugHandler)
import Web.Wheb.Routes (generateUrl, getParam)
import Web.Wheb.Types (CSettings, EResponse, HandlerData(..), 
                       HandlerResponse(HandlerResponse), InternalState(..), 
                       Route(..), RouteParamList, SettingsValue(..), 
                       UrlBuildError(UrlNameNotFound), WhebError(..), 
                       WhebFile(WhebFile), WhebHandlerT, WhebOptions(..), WhebT(WhebT))
import Web.Wheb.Utils (lazyTextToSBS, sbsToLazyText)

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
getSettings = WhebT $ liftM (runTimeSettings . globalSettings) ask

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
setRawHeader (hn, hc) = WhebT $ modify insertHeader 
    where insertHeader is@(InternalState {..}) = 
            is { respHeaders = M.insert hn hc respHeaders }
 
-- | Set a header for the response
setHeader :: Monad m => TS.Text -> TS.Text -> WhebT g s m ()
setHeader hn hc = setRawHeader (mk $ TS.encodeUtf8 hn, TS.encodeUtf8 hc)

-- | Give filepath and content type to serve a file via lazy text.
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

-- | Give content type and Blaze Builder
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
throwRedirect c = throwError $ ErrorStatus status302 T.empty
    
-- * Running a Wheb Application

-- | Running a Handler with a custom Transformer
runRawHandlerT :: WhebOptions g s m ->
             (m (Either WhebError a) -> IO (Either WhebError a)) ->
             Request ->
             WhebT g s m a ->
             IO (Either WhebError a)
runRawHandlerT opts@(WhebOptions {..}) runIO r h = 
    runIO $ runDebugHandler opts h baseData
    where baseData = HandlerData startingCtx r ([], []) [] opts

-- | Convenience wrapper for 'runRawHandlerT' function in 'IO'
runRawHandler :: WhebOptions g s IO -> 
              WhebT g s IO a ->
              IO (Either WhebError a)
runRawHandler opts h = runRawHandlerT opts id defaultRequest h

-- | Run a server with a function to run your inner Transformer to IO and 
-- generated options
runWhebServerT :: (forall a . m a -> IO a) ->
                  WhebOptions g s m ->
                  IO ()
runWhebServerT runIO opts@(WhebOptions {..}) = do
    putStrLn $ "Now running on port " ++ (show $ port)

    forceTVar <- newTVarIO False

    installHandler sigINT catchSig Nothing
    installHandler sigTERM catchSig Nothing

    forkIO $ runSettings rtSettings $
        gracefulExit $
        waiStack $
        optsToApplication opts runIO

    let termSig = (Catch (atomically $ writeTVar forceTVar True >> writeTVar shutdownTVar True))
        installForceKill = installHandler sigTERM termSig Nothing >> installHandler sigINT termSig Nothing

    loop installForceKill
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
        port = fromMaybe 3000 $ 
          (M.lookup (TS.pack "port") runTimeSettings) >>= (\(MkVal m) -> cast m)
        rtSettings = W.setPort port warpSettings

-- | Convenience wrapper for 'runWhebServerT' function in IO
runWhebServer :: (WhebOptions g s IO) -> IO ()
runWhebServer = runWhebServerT id
