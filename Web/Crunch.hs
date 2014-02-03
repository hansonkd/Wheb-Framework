{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Web.Crunch where

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
import           Data.Typeable (Typeable)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.URI
import           Web.Crunch.Types
import           Web.Crunch.Routes

lazyTextToSBS = TS.encodeUtf8 . T.toStrict
sbsToLazyText = T.fromStrict . TS.decodeUtf8

----------------------- Instances ------------------------
instance CrunchResponse Builder where
  toResponse = responseBuilder

instance CrunchResponse T.Text where
  toResponse s hds = responseBuilder s hds . fromLazyByteString . T.encodeUtf8

----------------------- CrunchT Functions ------------------------
getApp :: Monad m => CrunchT g s m g
getApp = CrunchT $ liftM globalCtx ask

getWithApp :: Monad m => (g -> a) -> CrunchT g s m a
getWithApp = flip liftM getApp

getRouteParams :: Monad m => CrunchT g s m RouteParamList
getRouteParams = CrunchT $ liftM routeParams ask

getRouteParam :: (Typeable a, Monad m) => T.Text -> CrunchT g s m (Maybe a)
getRouteParam t = liftM (getParam t) getRouteParams

getRequest :: Monad m => CrunchT g s m Request
getRequest = CrunchT $ liftM request ask

getWithRequest :: Monad m => (Request -> a) -> CrunchT g s m a
getWithRequest = flip liftM getRequest

getRawPOST :: MonadIO m => CrunchT g s m ([Param], [File LBS.ByteString])
getRawPOST = getRequest >>= (liftIO . (parseRequestBody lbsBackEnd))

getPOSTParams :: MonadIO m => CrunchT g s m [(T.Text, T.Text)]
getPOSTParams = liftM (fmap f . fst) getRawPOST
  where f (a, b) = (sbsToLazyText a, sbsToLazyText b)

getPostParam :: MonadIO m => T.Text -> CrunchT g s m (Maybe T.Text)
getPostParam k = liftM (lookup k) getPOSTParams 
  
getQuery :: Monad m => CrunchT g s m Query
getQuery = getWithRequest queryString

getHeader :: Monad m => T.Text -> CrunchT g s m (Maybe T.Text)
getHeader k = getRequest >>= f
  where hk = mk $ lazyTextToSBS k
        f = (return . (fmap sbsToLazyText) . (lookup hk) . requestHeaders)

setRawHeader :: Monad m => Header -> CrunchT g s m ()
setRawHeader (hn, hc) = CrunchT $ modify insertHeader 
    where insertHeader is@(InternalState {..}) = 
            is { respHeaders = M.insert hn hc respHeaders }

setHeader :: Monad m => T.Text -> T.Text -> CrunchT g s m ()
setHeader hn hc = setRawHeader (mk $ lazyTextToSBS hn, lazyTextToSBS hc)

html :: Monad m => T.Text -> CrunchHandler g s m
html c = do
    setHeader "Content-Type" "text/html" 
    return $ HandlerResponse status200 c

-----------------------Initialize Monad Functions ----------------
rGET :: UrlPat -> CrunchHandler g s m -> Route g s m
rGET p = Route Nothing (==GET) (compilePat p)

rPOST :: UrlPat -> CrunchHandler g s m -> Route g s m
rPOST p = Route Nothing (==POST) (compilePat p)

addGET :: UrlPat -> CrunchHandler g s m -> InitM g s m ()
addGET p h = addRoute $ rGET p h
 
addPOST :: UrlPat -> CrunchHandler g s m -> InitM g s m ()
addPOST p h = addRoute $ rPOST p h

catchAllRoutes :: CrunchHandler g s m -> InitM g s m ()
catchAllRoutes h = addRoute $
        Route Nothing (const True) (UrlParser $ const (Just [])) h

addRoute :: Route g s m -> InitM g s m ()
addRoute r = addRoutes [r]

addRoutes :: [Route g s m] -> InitM g s m ()
addRoutes rs = InitM $ tell $ mempty { initRoutes = rs }

addSetting :: T.Text -> T.Text -> InitM g s m ()
addSetting k v = addSettings $ M.fromList [(k, v)]

addSettings :: CSettings -> InitM g s m ()
addSettings settings = InitM $ tell $ mempty { initSettings = settings }

----------------------- Some defaults -----------------------
defaultErr :: Monad m => CrunchError -> CrunchHandler g s m
defaultErr err = return $ HandlerResponse status500 $ fromLazyByteString 
    ("<h1>Error: " <> (fromString $ show err) <> ".</h1>")

uhOh :: Response
uhOh = responseLBS status500 [("Content-Type", "text/html")]
      "Something went wrong on the server."

----------------------- Running the application -----------------------
runOpts :: (Default s) => CrunchOptions g s m ->
                          (m EResponse -> IO EResponse) ->
                          Request ->
                          IO Response
runOpts opts@(CrunchOptions {..}) runIO r = do
  res <- case (findUrlMatch stdMthd pathChunks appRoutes) of
            Just (h, params) -> runIO $ runCrunchHandler opts h params r
            Nothing          -> return $ Left Error404
  either handleError return res
  where pathChunks = fmap T.fromStrict $ pathInfo r
        stdMthd = either (\_-> GET) id $ parseMethod $ requestMethod r
        handleError err = do
          errRes <- runIO $ runCrunchHandler opts (defaultErrorHandler err) [] r
          either (return . (const uhOh)) return errRes

runCrunchServerT :: (Default s) => 
                  (m EResponse -> IO EResponse) ->
                  CrunchOptions g s m ->
                  IO ()
runCrunchServerT runIO opts = run 8080 $ runOpts opts runIO

runCrunchServer :: (Default s) => 
                 (CrunchOptions g s IO) ->
                 IO ()
runCrunchServer = runCrunchServerT id

generateOptions :: MonadIO m => InitM g s m g -> IO (CrunchOptions g s m)
generateOptions m = do 
  (g, InitOptions {..}) <- runWriterT (runInitM m)
  return $ CrunchOptions { appRoutes = initRoutes
                         , runTimeSettings = initSettings
                         , startingCtx = g
                         , defaultErrorHandler = defaultErr }

----------------------- Internal stuff -----------------------
runCrunchHandler :: (Default s) =>
                    CrunchOptions g s m ->
                    CrunchHandler g s m ->
                    RouteParamList ->
                    Request -> 
                    m EResponse
runCrunchHandler opts@(CrunchOptions {..}) handler params r = do
  (resp, InternalState {..}) <- flip runStateT def $ do
            flip runReaderT (HandlerData startingCtx r params opts) $
              runErrorT $
              runCrunchT handler
  return $ fmap (convertResponse respHeaders) resp 
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp