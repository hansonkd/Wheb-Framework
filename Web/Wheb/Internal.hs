{-# LANGUAGE RecordWildCards #-}

module Web.Wheb.Internal where

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

import           Web.Wheb.Routes
import           Web.Wheb.Types
import           Web.Wheb.Utils

runDebugHandler :: (Default s, Monad m) =>
                    WhebOptions g s m ->
                    WhebT g s m a  ->
                    HandlerData g s m ->
                    m (Either WhebError a)
runDebugHandler opts@(WhebOptions {..}) handler hd = do
  flip evalStateT def $ do
            flip runReaderT hd $
              runErrorT $
              runWhebT handler
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp
                          
                          
optsToApplication :: (Default s) => WhebOptions g s m ->
                     (m EResponse -> IO EResponse) ->
                     Application
optsToApplication opts@(WhebOptions {..}) runIO r = do
  pData <- liftIO (parseRequestBody lbsBackEnd r)
  res <- runIO $ do
          let mwData = baseData { postData = pData }
          (mRes, st) <- runMiddlewares opts whebMiddlewares mwData
          case mRes of
              Just resp -> return $ Right resp
              Nothing -> case (findUrlMatch stdMthd pathChunks appRoutes) of
                        Just (h, params) -> do
                            let hData = mwData { routeParams = params }
                            runWhebHandler opts h st hData 
                        Nothing          -> return $ Left Error404
  either handleError return res
  where baseData = HandlerData startingCtx r ([], []) [] opts
        pathChunks = fmap T.fromStrict $ pathInfo r
        stdMthd = either (\_-> GET) id $ parseMethod $ requestMethod r
        handleError err = do
          errRes <- runIO $ 
                      runWhebHandler opts (defaultErrorHandler err) def baseData
          either (return . (const uhOh)) return errRes
          
runMiddlewares :: (Default s, Monad m) =>
                  WhebOptions g s m ->
                  [WhebMiddleware g s m] ->
                  HandlerData g s m ->
                  m (Maybe Response, InternalState s)
runMiddlewares opts mWs hd = loop mWs def
    where loop [] st = return (Nothing, st)
          loop (mw:mws) st = do
                  mwResult <-  (runWhebMiddleware opts st hd mw)
                  case mwResult of
                        (Just resp, nst) -> return mwResult
                        (Nothing, nst)   -> loop mws nst

runWhebMiddleware :: (Default s, Monad m) =>
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
                          
runWhebHandler :: (Default s, Monad m) =>
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
                          