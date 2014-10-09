{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Web.Wheb.Internal where

import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.State (evalStateT, StateT(runStateT))
import qualified Data.Map as M (toList)
import Network.HTTP.Types.Method (parseMethod, StdMethod(GET))
import Network.Wai (Application, Request(..), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as W
import Web.Wheb.Routes (findUrlMatch, findSiteMatch, findSocketMatch)
import Web.Wheb.Types
import Web.Wheb.Utils (uhOh)
import Web.Cookie (parseCookiesText)

-- * Converting to WAI application
                      
-- | Convert 'WhebOptions' to 'Application'                        
optsToApplication :: WhebOptions g s m ->
                     (forall a. m a -> IO a) ->
                     Application
optsToApplication opts@(WhebOptions {..}) runIO r respond = do
  if ((length appWhebSockets) > 0)
    then websocketsOr W.defaultConnectionOptions socketHandler handleMain r respond
    else handleMain r respond

  where socketHandler pc = do
              case (findSocketMatch pathChunks appWhebSockets) of
                  Just (h, params) -> do
                      c <- W.acceptRequest pc
                      void $ runIO $ do
                            (mRes, st) <- runMiddlewares initOpts whebMiddlewares baseData
                            runDebugHandler (initOpts {startingState = st}) (h c) (baseData { routeParams = params })
                  Nothing -> W.rejectRequest pc (B.pack "No socket for path.")

        handleMain r' respond' = do
            pData <- parseRequestBody lbsBackEnd r'
            res <- runIO $ do
                    let mwData = baseData { postData = pData }
                    (mRes, st) <- runMiddlewares initOpts whebMiddlewares mwData
                    case mRes of
                        Just resp -> return $ Right resp
                        Nothing -> do
                            case (findSiteMatch appSites pathChunks) of
                              Just h -> do
                                runWhebHandler initOpts h st mwData
                              Nothing -> do
                                  case (findUrlMatch stdMthd pathChunks appRoutes) of
                                        Just (h, params) -> do
                                            let hData = mwData { routeParams = params }
                                            runWhebHandler opts h st hData 
                                        Nothing          -> return $ Left Error404
            finished <- either handleError return res
            respond' finished
        baseData   = HandlerData startingCtx r ([], []) [] opts
        parsedCookies = parseCookiesText $ (fromMaybe B.empty) $ (lookup $ CI.mk $ B.pack "Cookie") $ requestHeaders r
        initOpts = opts {startingState = startingState {curCookies = parsedCookies}}
        pathChunks = pathInfo r
        stdMthd    = either (\_-> GET) id $ parseMethod $ requestMethod r
        runErrorHandler eh = runWhebHandler initOpts eh startingState baseData
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
runWhebHandler (WhebOptions {..}) handler st hd = do
  (resp, InternalState {..}) <- flip runStateT st $ do
            flip runReaderT hd $
              runExceptT $
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
              runExceptT $
              runWhebT handler

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
                        (Just _, _) -> return mwResult
                        (Nothing, nst)   -> loop mws nst

runWhebMiddleware :: Monad m =>
                    WhebOptions g s m ->
                    InternalState s ->
                    HandlerData g s m ->
                    WhebMiddleware g s m ->
                    m (Maybe Response, InternalState s)
runWhebMiddleware (WhebOptions {..}) st hd mW = do
        (eresp, is@InternalState {..}) <- flip runStateT st $ do
                  flip runReaderT hd $
                    runExceptT $
                    runWhebT mW
        return $ (convertResponse respHeaders eresp, is)
  where convertResponse hds (Right (Just (HandlerResponse status resp))) =
                              Just (toResponse status (M.toList hds) resp)
        convertResponse _ _ = Nothing
