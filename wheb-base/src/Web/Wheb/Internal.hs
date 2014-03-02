{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.Internal where

import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Parse

import           Web.Routes (runSite, Site(..))
import           Web.Wheb.Routes
import           Web.Wheb.Types
import           Web.Wheb.Utils

findSiteMatch :: [PackedSite g s m] -> 
                 [T.Text] -> 
                 Maybe (WhebHandlerT g s m)
findSiteMatch [] _ = Nothing
findSiteMatch ((PackedSite t site):sites) cs = 
  either (const (findSiteMatch sites cs)) Just $
        runSite (T.toStrict t) site (map T.toStrict cs)

-- * Converting to WAI application
                      
-- | Convert 'WhebOptions' to 'Application'                        
optsToApplication :: WhebOptions g s m ->
                     (m EResponse -> IO EResponse) ->
                     Application
optsToApplication opts@(WhebOptions {..}) runIO r respond = do
  pData <- parseRequestBody lbsBackEnd r
  res <- runIO $ do
          let mwData = baseData { postData = pData }
          (mRes, st) <- runMiddlewares opts whebMiddlewares mwData
          case mRes of
              Just resp -> return $ Right resp
              Nothing -> do
                  case (findSiteMatch appSites pathChunks) of
                    Just h -> do
                      runWhebHandler opts h st mwData
                    Nothing -> do
                        case (findUrlMatch stdMthd pathChunks appRoutes) of
                              Just (h, params) -> do
                                  let hData = mwData { routeParams = params }
                                  runWhebHandler opts h st hData 
                              Nothing          -> return $ Left Error404

  finished <- either handleError return res
  respond finished
  where baseData   = HandlerData startingCtx r ([], []) [] opts
        pathChunks = fmap T.fromStrict $ pathInfo r
        stdMthd    = either (\_-> GET) id $ parseMethod $ requestMethod r
        runErrorHandler eh = runWhebHandler opts eh startingState baseData
        handleError err = do
          errRes <- runIO $ runErrorHandler (defaultErrorHandler err)
          either (return . (const uhOh)) return errRes

-- * Running Handlers

-- | Run all inner wheb monads to the top level.
runWhebHandler :: Monad m =>
                    WhebOptions g s m ->
                    WhebHandlerT g s m ->
                    InternalState s ->
                    HandlerData g s m ->
                    m EResponse
runWhebHandler opts@(WhebOptions {..}) handler st hd = do
  (resp, InternalState {..}) <- flip runStateT st $ do
            flip runReaderT hd $
              runErrorT $
              runWhebT handler
  return $ fmap (convertResponse respHeaders) resp 
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp

-- | Same as above but returns arbitrary type for debugging.
runDebugHandler :: Monad m =>
                    WhebOptions g s m ->
                    WhebT g s m a  ->
                    HandlerData g s m ->
                    m (Either WhebError a)
runDebugHandler opts@(WhebOptions {..}) handler hd = do
  flip evalStateT startingState $ do
            flip runReaderT hd $
              runErrorT $
              runWhebT handler
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp
-- * Running Middlewares
 
-- | Runs middlewares in order, stopping if one returns a response
runMiddlewares :: Monad m =>
                  WhebOptions g s m ->
                  [WhebMiddleware g s m] ->
                  HandlerData g s m ->
                  m (Maybe Response, InternalState s)
runMiddlewares opts mWs hd = loop mWs (startingState opts)
    where loop [] st = return (Nothing, st)
          loop (mw:mws) st = do
                  mwResult <-  (runWhebMiddleware opts st hd mw)
                  case mwResult of
                        (Just resp, nst) -> return mwResult
                        (Nothing, nst)   -> loop mws nst

runWhebMiddleware :: Monad m =>
                    WhebOptions g s m ->
                    InternalState s ->
                    HandlerData g s m ->
                    WhebMiddleware g s m ->
                    m (Maybe Response, InternalState s)
runWhebMiddleware opts@(WhebOptions {..}) st hd mW = do
        (eresp, is@InternalState {..}) <- flip runStateT st $ do
                  flip runReaderT hd $
                    runErrorT $
                    runWhebT mW
        return $ (convertResponse respHeaders eresp, is)
  where convertResponse hds (Right (Just (HandlerResponse status resp))) =
                              Just (toResponse status (M.toList hds) resp)
        convertResponse _ _ = Nothing
