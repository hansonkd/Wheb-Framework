{-# LANGUAGE RecordWildCards #-}
module Web.Crunchy.Internal where

import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Default
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Parse

import           Web.Crunchy.Routes
import           Web.Crunchy.Types
import           Web.Crunchy.Utils

runDebugHandler :: (Default s, Monad m) =>
                    CrunchyOptions g s m ->
                    CrunchyT g s m a  ->
                    HandlerData g s m ->
                    m (Either CrunchyError a)
runDebugHandler opts@(CrunchyOptions {..}) handler hd = do
  flip evalStateT def $ do
            flip runReaderT hd $
              runErrorT $
              runCrunchyT handler
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp
                          
                          
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
                          