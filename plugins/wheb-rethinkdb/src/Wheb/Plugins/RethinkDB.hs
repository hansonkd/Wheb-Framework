{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

{- |
Basic implementation of RethinkDB connection pool.

Adds instance for 'SessionApp' for 'RethinkApp'.

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
import           Data.Pool
import           Wheb
import           Wheb.Plugins.Session



data RethinkContainer = RethinkContainer (Pool RethinkDBHandle)

class RethinkApp a where
    getRethinkContainer :: a -> RethinkContainer

instance RethinkApp a => SessionApp a where
    type SessionAppBackend a = RethinkContainer
    getSessionContainer = getRethinkContainer


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


handleEither :: Monad m => Either RethinkDBError b -> WhebT g s m b
handleEither = either (throwError . Error500 . TL.pack . show) return

-- | Push an error from RethinkDB to a 500 Error.
catchResult :: MonadIO m => IO b -> WhebT g s m b
catchResult m = (liftIO $ try m) >>= handleEither

mvoid :: MonadIO m => IO b -> WhebT g s m ()
mvoid m = catchResult m >> return ()

getSessionTable :: Monad m => WhebT g s m Table
getSessionTable = table `liftM` getSetting'' "session-table" "sessions"


getContainer :: (RethinkApp g, Monad m) => WhebT g s m RethinkContainer
getContainer = getWithApp getRethinkContainer

runPool :: (Expr query, Result r) => Pool RethinkDBHandle -> query -> IO r
runPool p q = withResource p $ \h -> run h q

runWithContainer :: (Expr query, Result r) => RethinkContainer -> query ->  IO r
runWithContainer (RethinkContainer handle) = runPool handle

runToDatum :: Expr query => RethinkContainer -> query -> IO (Maybe Datum) 
runToDatum (RethinkContainer handle) = runPool handle

runDb :: (Expr expr, Result r, MonadIO m, RethinkApp g) => expr -> WhebT g s m r
runDb expr = getContainer >>= (\c -> liftIO $ runWithContainer c expr)

runDb' :: (Expr expr, MonadIO m, RethinkApp g) => expr -> WhebT g s m Datum
runDb' expr = getContainer >>= (\(RethinkContainer p) -> liftIO $ withResource p $ \h -> run' h expr)

runOpts :: (Expr expr, MonadIO m, RethinkApp g) => [RunFlag] -> expr -> WhebT g s m Datum
runOpts opts expr = getContainer >>= (\(RethinkContainer p) -> liftIO $ withResource p $ \h -> R.runOpts h opts expr)


-- | Initialize rethinkDB with \"host\" \"post\" \"password\" and default database.
initRethinkDB :: T.Text -> Integer -> Maybe T.Text -> Maybe T.Text -> InitM g s m RethinkContainer
initRethinkDB host port password defaultDB = do
    pool <- liftIO connectDbPool
    return $ RethinkContainer pool
    where connectDbPool = createPool connectToDb close 1 10 5
          connectToDb = do
            handle <- connect (T.unpack host) port (fmap T.unpack password)
            return $ maybe handle (flip use handle . db) defaultDB
