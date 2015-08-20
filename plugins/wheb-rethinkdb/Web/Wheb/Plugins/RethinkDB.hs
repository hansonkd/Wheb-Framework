{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

{- |
Basic implementation of MongoDB connection.

Adds default instances for 'SessionApp' and 'AuthApp' for 'RethinkApp'.

-}

module Wheb.Plugins.RethinkDB (
      runDb
    , runDb'
    , runOpts
    , initRethinkDB
    , catchResult
    , RethinkApp (..)
    , RethinkContainer (..)
    , module Database.RethinkDB.NoClash
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except (throwError)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.RethinkDB as R
import           Database.RethinkDB.NoClash hiding (runOpts)
import           Wheb
import           Wheb.Plugins.Session
import           Wheb.Plugins.Auth

data RethinkContainer = RethinkContainer RethinkDBHandle

class RethinkApp a where
    getRethinkContainer :: a -> RethinkContainer

instance RethinkApp a => SessionApp a where
    getSessionContainer = SessionContainer . getRethinkContainer

instance RethinkApp a => AuthApp a where
    getAuthContainer = AuthContainer . getRethinkContainer

instance SessionBackend RethinkContainer where
  backendSessionPut sessId key content mc = do
    sessionTable <- getSessionTable
    mvoid $ runToDatum mc $
      sessionTable # get (literal sessId) # update (\row -> merge row ["sessId" := sessId, key := content])
  backendSessionGet sessId key mc =  do
    sessionTable <- getSessionTable
    catchResult $ runWithContainer mc (sessionTable # getAll "sessId" [sessId] # (!(literal key))) >>= next 
  backendSessionDelete sessId key mc = do
    sessionTable <- getSessionTable
    mvoid $ runToDatum mc $
      sessionTable # getAll "sessId" [sessId] # merge [key := remove]
  backendSessionClear sessId mc = do
    sessionTable <- getSessionTable
    mvoid $ runToDatum mc $
      sessionTable # getAll "sessId" [sessId] # delete

instance AuthBackend RethinkContainer where
  backendGetUser name mc = do
    authTable <- getAuthTable
    user <- catchResult $ runToDatum mc $ authTable # get (expr name)
    return $ fmap (const $ AuthUser name) user
  backendLogin name pw mc =  do
    authTable <- getAuthTable
    user <- catchResult $ runWithContainer mc $ authTable # get (expr name) # (!"password")
    
    case fmap (\doc -> (verifyPw pw doc)) user of
        Just True  -> return (Right $ AuthUser $ name)
        Just False -> return (Left InvalidPassword)
        Nothing    -> return (Left UserDoesNotExist)
  backendRegister user@(AuthUser name) pw mc =  do
    authTable <- getAuthTable
    pwHash <- makePwHash pw
    muser <- catchResult $ runToDatum mc $ authTable # get (expr name)
    case muser of
        Just _ -> return (Left DuplicateUsername)
        Nothing -> do
            catchResult $ runToDatum mc $ 
                        authTable # insert  [ "username" := (name)
                                            , "password" := (pwHash)]
            return (Right user)
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

handleEither :: Monad m => Either RethinkDBError b -> WhebT g s m b
handleEither = either (throwError . Error500 . TL.pack . show) return

-- | Push an error from RethinkDB to a 500 Error.
catchResult :: MonadIO m => IO b -> WhebT g s m b
catchResult m = (liftIO $ try m) >>= handleEither

mvoid :: MonadIO m => IO b -> WhebT g s m ()
mvoid m = catchResult m >> return ()

getSessionTable :: Monad m => WhebT g s m Table
getSessionTable = table `liftM` getSetting'' "session-table" "sessions"

getAuthTable :: Monad m => WhebT g s m Table
getAuthTable = table `liftM` getSetting'' "auth-table" "users"

getContainer :: (RethinkApp g, Monad m) => WhebT g s m RethinkContainer
getContainer = getWithApp getRethinkContainer

runWithContainer :: (Expr query, Result r) => RethinkContainer -> query ->  IO r
runWithContainer (RethinkContainer handle) action = R.run handle action

runToDatum :: Expr query => RethinkContainer -> query -> IO (Maybe Datum) 
runToDatum (RethinkContainer handle) action = R.run handle action

runDb :: (Expr expr, Result r, MonadIO m, RethinkApp g) => expr -> WhebT g s m r
runDb expr = (getContainer) >>= (\c -> liftIO $ runWithContainer c $ expr)

runDb' :: (Expr expr, MonadIO m, RethinkApp g) => expr -> WhebT g s m Datum
runDb' expr = (getContainer) >>= (\(RethinkContainer h) -> liftIO $ R.run' h expr)

runOpts :: (Expr expr, MonadIO m, RethinkApp g) => [RunFlag] -> expr -> WhebT g s m Datum
runOpts opts expr = (getContainer) >>= (\(RethinkContainer h) -> liftIO $ R.runOpts h opts expr)

ensureAuthTable :: Monad m => WhebT g s m ()
ensureAuthTable = do
    authTable <- getAuthTable
    return ()

-- | Initialize rethinkDB with \"host\" \"post\" \"password\" and default database.
initRethinkDB :: T.Text -> Integer -> (Maybe T.Text) -> (Maybe T.Text) -> InitM g s m RethinkContainer
initRethinkDB host port password defaultDB = do
    handle <- liftIO $ connect (T.unpack host) port (fmap T.unpack password)
    addCleanupHook $ close handle
    return $ RethinkContainer $ maybe handle (flip use handle . db) defaultDB
