{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes      #-}

module Wheb.Internal where

import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (fromMaybe)
import           Control.Monad (void)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Reader (ReaderT(runReaderT))
import           Control.Monad.State.Strict (evalStateT, StateT(runStateT))
import           Control.Monad.Logger (runStdoutLoggingT)

import qualified Data.Map as M (toList)
import           Network.HTTP.Types.Method (parseMethod, StdMethod(GET))
import           Network.Wai (Application, Request(..), Response)
import           Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as W
import           Wheb.Routes (findUrlMatch, findSiteMatch, findSocketMatch)
import           Wheb.Types
import           Wheb.Utils (uhOh)
import           Web.Cookie (parseCookiesText)

-- * Converting to WAI application
                      
-- | Convert 'WhebOptions' to 'Application'                        
optsToApplication :: WhebOptions g s m ->
                     Application
optsToApplication opts@(WhebOptions {..}) r respond =
  if not $ null appWhebSockets
    then websocketsOr W.defaultConnectionOptions socketHandler handleMain r respond
    else handleMain r respond

  where socketHandler pc =
          case findSocketMatch pathChunks appWhebSockets of
              Just (h, params) -> do
                  c <- W.acceptRequest pc
                  void $ runToIO $ do
                        (_, st) <- runMiddlewares initOpts whebMiddlewares baseData
                        runDebugHandler (initOpts {startingState = st}) (h c) (baseData { routeParams = params })
              Nothing -> W.rejectRequest pc (B.pack "No socket for path.")

        handleMain r' respond' = do
            pData <- parseRequestBody lbsBackEnd r'
            res <- runToIO $ do
                let mwData = baseData { postData = pData }
                (mRes, st) <- runMiddlewares initOpts whebMiddlewares mwData
                case mRes of
                    Just resp -> return $ Right resp
                    Nothing ->
                        case findSiteMatch appSites pathChunks of
                          Just h -> runWhebHandler initOpts h st mwData
                          Nothing ->
                              case findUrlMatch stdMthd pathChunks appRoutes of
                                    Just (h, params) -> do
                                        let hData = mwData { routeParams = params }
                                        runWhebHandler opts h st hData
                                    Nothing          -> return $ Left Error404
            finished <- either handleError return res
            respond' finished
        baseData   = HandlerData startingCtx r ([], []) [] runTimeSettings (map routeInfo appRoutes)
        parsedCookies = parseCookiesText $ fromMaybe B.empty $ lookup (CI.mk $ B.pack "Cookie") $ requestHeaders r
        initOpts = opts {startingState = startingState {curCookies = parsedCookies}}
        pathChunks = pathInfo r
        stdMthd    = either (const GET) id $ parseMethod $ requestMethod r
        runErrorHandler eh = runWhebHandler initOpts eh startingState baseData
        handleError err = do
          errRes <- runToIO $ runErrorHandler (defaultErrorHandler err)
          either (return . const uhOh) return errRes

-- * Running Handlers

-- | Run all inner wheb monads to the top level.
runWhebHandler :: Monad m =>
                    WhebOptions g s m ->
                    WhebHandlerT g s m ->
                    InternalState s ->
                    HandlerData g ->
                    m EResponse
runWhebHandler (WhebOptions {..}) handler st hd = do
  (resp, InternalState {..}) <- flip runStateT st $
            flip runReaderT hd $
              runExceptT $
              runStdoutLoggingT $
              runWhebT handler
  return $ fmap (convertResponse respHeaders) resp 
  where convertResponse hds (HandlerResponse status resp) =
                          toResponse status (M.toList hds) resp

-- | Same as 'runWhebHandler' but returns arbitrary type for debugging.
runDebugHandler :: Monad m =>
                    WhebOptions g s m ->
                    WhebT g s m a  ->
                    HandlerData g ->
                    m (Either WhebError a)
runDebugHandler (WhebOptions {..}) handler hd =
  flip evalStateT startingState $
            flip runReaderT hd $
              runExceptT $
              runStdoutLoggingT $
              runWhebT handler

-- * Running Middlewares
 
-- | Runs middlewares in order, stopping if one returns a response
runMiddlewares :: Monad m =>
                  WhebOptions g s m ->
                  [WhebMiddleware g s m] ->
                  HandlerData g ->
                  m (Maybe Response, InternalState s)
runMiddlewares opts mWs hd = loop mWs (startingState opts)
    where loop [] st = return (Nothing, st)
          loop (mw:mws) st = do
                  mwResult <- runWhebMiddleware opts st hd mw
                  case mwResult of
                        (Just _, _) -> return mwResult
                        (Nothing, nst)   -> loop mws nst

runWhebMiddleware :: Monad m =>
                    WhebOptions g s m ->
                    InternalState s ->
                    HandlerData g ->
                    WhebMiddleware g s m ->
                    m (Maybe Response, InternalState s)
runWhebMiddleware (WhebOptions {..}) st hd mW = do
        (eresp, is@InternalState {..}) <- flip runStateT st $
                  flip runReaderT hd $
                    runExceptT $
                    runStdoutLoggingT $
                    runWhebT mW
        return (convertResponse respHeaders eresp, is)
  where convertResponse hds (Right (Just (HandlerResponse status resp))) =
                              Just (toResponse status (M.toList hds) resp)
        convertResponse _ _ = Nothing
