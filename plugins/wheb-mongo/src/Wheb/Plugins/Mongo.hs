{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

{- |
Basic implementation of MongoDB connection.

Adds default instances for 'SessionApp' and 'AuthApp' for 'MongoApp'.

-}

module Wheb.Plugins.Mongo (
      runAction
    , initMongo
    , catchResult
    , getSessionCollection
    , MongoApp (..)
    , MongoContainer
    , module Database.MongoDB
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except (throwError)
import           Data.Bson as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Pool
import           Database.MongoDB
import           Wheb
import           Wheb.Plugins.Session

data MongoContainer = MongoContainer (Pool Pipe) AccessMode Database

class MongoApp a where
    getMongoContainer :: a -> MongoContainer

instance MongoApp a => SessionApp a where
    type SessionAppBackend a = MongoContainer
    getSessionContainer = getMongoContainer

instance SessionBackend MongoContainer where
  backendSessionPut sessId key content mc = do
    collectionName <- getSessionCollection
    mvoid $ runWithContainer mc $
      insert_ collectionName [ "sessId" := (toBsonString sessId)
                             , key := (toBsonString content) ]
  backendSessionGet sessId key mc =  do
    collectionName <- getSessionCollection
    catchResult $ runWithContainer mc $ do
      n <- next =<< find (select ["sessId" := (toBsonString sessId)] collectionName)
      return $ maybe Nothing (B.lookup (key)) n
  backendSessionDelete sessId key mc = do
    collectionName <- getSessionCollection
    mvoid $ runWithContainer mc $
      modify (select ["sessId" := (toBsonString sessId)] collectionName) ["$unset" := val [key := toBsonString ""]]
  backendSessionClear sessId mc = do
    collectionName <- getSessionCollection
    mvoid $ runWithContainer mc $
      delete (select ["sessId" := (toBsonString sessId)] collectionName)

toBsonString = val

handleEither :: Monad m => Either Failure b -> WhebT g s m b
handleEither = either (throwError . Error500 . TL.pack . show) return

-- | Push an error from Mongo to a 500 Error.
catchResult :: MonadIO m => IO b -> WhebT g s m b
catchResult m = (liftIO $ try m) >>= handleEither

mvoid :: MonadIO m => IO b -> WhebT g s m ()
mvoid m = catchResult m >> return ()

getSessionCollection :: Monad m => WhebT g s m Collection
getSessionCollection = getSetting'' "session-collection" "sessions"

runWithContainer :: MongoContainer -> Action IO a ->  IO a
runWithContainer (MongoContainer pool mode db) action = withResource pool $ \h -> access h mode db action

-- | Run a MongoDB Action Monad in WhebT
runAction :: (MongoApp g, MonadIO m) => Action IO a -> WhebT g s m a
runAction action = getWithApp getMongoContainer >>= (\c -> liftIO $ runWithContainer c action)

-- | Initialize mongo with \"host:post\" and default database.
initMongo :: T.Text -> T.Text -> InitM g s m MongoContainer
initMongo host db = do
    pool <- liftIO $ createPool connectToDb close 1 10 5
    return $ MongoContainer pool master db
    where hp = readHostPort $ T.unpack host
          connectToDb = connect hp
