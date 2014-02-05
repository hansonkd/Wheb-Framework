{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Web.Crunchy where

import           Blaze.ByteString.Builder (Builder, fromLazyByteString)
import           Data.CaseInsensitive (mk)
import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Default
import           Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS
import           Data.String (fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as TS
import           Data.Typeable (Typeable, cast)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.URI
import           Web.Crunchy.Types
import           Web.Crunchy.Routes

lazyTextToSBS = TS.encodeUtf8 . T.toStrict
sbsToLazyText = T.fromStrict . TS.decodeUtf8

----------------------- Instances ------------------------
instance CrunchyContent Builder where
  toResponse = responseBuilder

instance CrunchyContent T.Text where
  toResponse s hds = responseBuilder s hds . fromLazyByteString . T.encodeUtf8

----------------------- CrunchyT Functions ------------------------
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

getQuery :: Monad m => CrunchyT g s m Query
getQuery = getWithRequest queryString

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
    setHeader "Content-Type" "text/html" 
    return $ HandlerResponse status200 c

-----------------------Initialize Monad Functions ----------------
rGET :: UrlPat -> CrunchyHandler g s m -> Route g s m
rGET p = Route Nothing (==GET) (compilePat p)

rPOST :: UrlPat -> CrunchyHandler g s m -> Route g s m
rPOST p = Route Nothing (==POST) (compilePat p)

addGET :: UrlPat -> CrunchyHandler g s m -> InitM g s m ()
addGET p h = addRoute $ rGET p h

addPOST :: UrlPat -> CrunchyHandler g s m -> InitM g s m ()
addPOST p h = addRoute $ rPOST p h

addWAIMiddleware :: Middleware -> InitM g s m ()
addWAIMiddleware m = InitM $ tell $ mempty { initWaiMw = m }

addCrunchyMiddleware :: CrunchyMiddleware g s m -> InitM g s m ()
addCrunchyMiddleware m = InitM $ tell $ mempty { initCrunchyMw = [m] }

catchAllRoutes :: CrunchyHandler g s m -> InitM g s m ()
catchAllRoutes h = addRoute $ Route Nothing (const True) parser h
        where parser = UrlParser (const (Just [])) (const (Right "/*"))

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

----------------------- Some defaults -----------------------
defaultErr :: Monad m => CrunchyError -> CrunchyHandler g s m
defaultErr err = return $ HandlerResponse status500 $ fromLazyByteString 
    ("<h1>Error: " <> (fromString $ show err) <> ".</h1>")

uhOh :: Response
uhOh = responseLBS status500 [("Content-Type", "text/html")]
      "Something went wrong on the server."

----------------------- Running the application -----------------------
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

----------------------- Internal stuff -----------------------
optsToApplication :: (Default s) => CrunchyOptions g s m ->
                     (m EResponse -> IO EResponse) ->
                     Application
optsToApplication opts@(CrunchyOptions {..}) runIO r = do
  pData <- liftIO (parseRequestBody lbsBackEnd r)
  res <- runIO $ do
          let mwData = baseData { postData = pData }
          (mRes, st) <- runMiddlewares opts crunchyMiddlewares mwData
          case mRes of
              Just resp -> return $ Right resp
              Nothing -> case (findUrlMatch stdMthd pathChunks appRoutes) of
                        Just (h, params) -> do
                            let hData = mwData { routeParams = params }
                            runCrunchyHandler opts h st hData 
                        Nothing          -> return $ Left Error404
  either handleError return res
  where baseData = HandlerData startingCtx r ([], []) [] opts
        pathChunks = fmap T.fromStrict $ pathInfo r
        stdMthd = either (\_-> GET) id $ parseMethod $ requestMethod r
        handleError err = do
          errRes <- runIO $ 
                      runCrunchyHandler opts (defaultErrorHandler err) def baseData
          either (return . (const uhOh)) return errRes
          
runMiddlewares :: (Default s, Monad m) =>
                  CrunchyOptions g s m ->
                  [CrunchyMiddleware g s m] ->
                  HandlerData g s m ->
                  m (Maybe Response, InternalState s)
runMiddlewares opts mWs hd = loop mWs def
    where loop [] st = return (Nothing, st)
          loop (mw:mws) st = do
                  mwResult <-  (runCrunchyMiddleware opts st hd mw)
                  case mwResult of
                        (Just resp, nst) -> return mwResult
                        (Nothing, nst)   -> loop mws nst

runCrunchyMiddleware :: (Default s, Monad m) =>
                    CrunchyOptions g s m ->
                    InternalState s ->
                    HandlerData g s m ->
                    CrunchyMiddleware g s m ->
                    m (Maybe Response, InternalState s)
runCrunchyMiddleware opts@(CrunchyOptions {..}) st hd mW = do
        (eresp, is@InternalState {..}) <- flip runStateT st $ do
                  flip runReaderT hd $
                    runErrorT $
                    runCrunchyT mW
        return $ (convertResponse respHeaders eresp, is)
  where convertResponse hds (Right (Just (HandlerResponse status resp))) =
                              Just (toResponse status (M.toList hds) resp)
        convertResponse _ _ = Nothing
                          
runCrunchyHandler :: (Default s, Monad m) =>
                    CrunchyOptions g s m ->
                    CrunchyHandler g s m ->
                    InternalState s ->
                    HandlerData g s m ->
                    m EResponse
runCrunchyHandler opts@(CrunchyOptions {..}) handler st hd = do
  (resp, InternalState {..}) <- flip runStateT st $ do
            flip runReaderT hd $
              runErrorT $
              runCrunchyT handler
  return $ fmap (convertResponse respHeaders) resp 
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp