{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

{- |
Basic implementation of MongoDB connection.

Adds default instances for 'SessionApp' and 'AuthApp' for 'MongoApp'.

You can override the collection names for the Auth and Session documents by using 'addSetting' and setting keys for \"session-collection\"
and \"auth-collection\"

> opts <- generateOptions $ do
>    addSetting "session-collection" "my-collection"

Reimplimentation of official example below. Use with language extensions /OvererloadedStrings/ & /ExtendedDefaultRules/.

>  import qualified Data.Text.Lazy as T
>  
>  import           Web.Wheb
>  import           Web.Wheb.Plugins.Mongo
>  
>  data MyApp = MyApp MongoContainer
>  data MyRequestState = MyRequestState
>  
>  instance MongoApp MyApp where
>      getMongoContainer (MyApp mc) = mc
>  
>  homePage :: WhebHandler MyApp MyRequestState
>  homePage = do
>      teams <- runAction $ rest =<< find (select [] "team") {sort = ["home.city" =: 1]}
>      text $ T.intercalate " | " $ map spack teams
>  
>  main :: IO ()
>  main = do
>    opts <- generateOptions $ do
>      addGET "." rootPat $ homePage
>      mongo <- initMongo "127.0.0.1:27017" "master"
>      return (MyApp mongo, MyRequestState)

>    runRawHandler opts $ do
>      runAction $ do
>          delete (select [] "team")
>          insertMany "team" [
>              ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
>              ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
>              ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
>              ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]
>  
>    runWhebServer opts
-}

module Web.Wheb.Plugins.Mongo (
      runAction
    , initMongo
    , catchResult
    , MongoApp (..)
    , MongoContainer
    , module Database.MongoDB
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except (throwError)
import           Data.Bson as B
import qualified Data.Text.Lazy as T
import           Database.MongoDB
import           Web.Wheb
import           Web.Wheb.Plugins.Session
import           Web.Wheb.Plugins.Auth

data MongoContainer = MongoContainer Pipe AccessMode Database

class MongoApp a where
    getMongoContainer :: a -> MongoContainer

instance MongoApp a => SessionApp a where
    getSessionContainer = SessionContainer . getMongoContainer

instance MongoApp a => AuthApp a where
    getAuthContainer = AuthContainer . getMongoContainer

instance SessionBackend MongoContainer where
  backendSessionPut sessId key content mc = do
    collectionName <- getSessionCollection
    mvoid $ runWithContainer mc $ do
      insert_ collectionName [ "sessId" := (toBsonString sessId)
                             , "key" := (toBsonString key)
                             , "content" := (toBsonString content) ]
  backendSessionGet sessId key mc =  do
    collectionName <- getSessionCollection
    catchResult $ runWithContainer mc $ do
      n <- next =<< find (select ["sessId" := (toBsonString sessId), "key" := (toBsonString key)] collectionName)
      return $ maybe Nothing ((fmap T.fromStrict) . (B.lookup (T.toStrict key))) n
  backendSessionDelete sessId key mc = do
    collectionName <- getSessionCollection
    mvoid $ runWithContainer mc $
      delete (select ["sessId" := (toBsonString sessId), "key" := (toBsonString key)] collectionName)
  backendSessionClear sessId mc = do
    collectionName <- getSessionCollection
    mvoid $ runWithContainer mc $
      delete (select ["sessId" := (toBsonString sessId)] collectionName)

instance AuthBackend MongoContainer where
  backendGetUser name mc = do
    collectionName <- getAuthCollection
    catchResult $ runWithContainer mc $ do
      n <- next =<< find (select ["username" := (toBsonString name)] collectionName)
      return $ maybe Nothing (const $ Just $ AuthUser name) n
  backendLogin name pw mc =  do
    collectionName <- getAuthCollection
    passCheck <- catchResult $ runWithContainer mc $ do
      n <- next =<< find (select ["username" := (toBsonString name)] collectionName)
      return $ maybe Nothing (\doc -> fmap (verifyPw pw . T.fromStrict) (B.lookup "password" doc)) n
    case passCheck of
        Just True  -> return (Right $ AuthUser $ name)
        Just False -> return (Left InvalidPassword)
        Nothing    -> return (Left UserDoesNotExist)
  backendRegister user@(AuthUser name) pw mc =  do
    collectionName <- getAuthCollection
    pwHash <- makePwHash pw
    catchResult $ runWithContainer mc $ do
      n <- next =<< find (select ["username" := (toBsonString name)] collectionName)
      case n of
        Just _ -> return (Left DuplicateUsername)
        Nothing -> do
          insert_ collectionName [ "username" := (toBsonString name)
                                 , "password" := (toBsonString pwHash)]
          return (Right user)
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

toBsonString = val . T.toStrict

handleEither :: Monad m => Either Failure b -> WhebT g s m b
handleEither = either (throwError . Error500 . show) return

-- | Push an error from Mongo to a 500 Error.
catchResult :: MonadIO m => IO b -> WhebT g s m b
catchResult m = (liftIO $ try m) >>= handleEither

mvoid :: MonadIO m => IO b -> WhebT g s m ()
mvoid m = catchResult m >> return ()

getSessionCollection :: Monad m => WhebT g s m Collection
getSessionCollection = liftM T.toStrict (getSetting'' "session-collection" "sessions")

getAuthCollection :: Monad m => WhebT g s m Collection
getAuthCollection = liftM T.toStrict (getSetting'' "auth-collection" "users")

runWithContainer :: MongoContainer -> Action IO a ->  IO a
runWithContainer (MongoContainer pipe mode db) action = liftIO $ access pipe mode db action

-- | Run a MongoDB Action Monad in WhebT
runAction :: (MongoApp g, MonadIO m) => 
             Action IO a -> 
             WhebT g s m a
runAction action = (getWithApp getMongoContainer) >>= (\c -> liftIO $ runWithContainer c action)

-- | Initialize mongo with \"host:post\" and default database.
initMongo :: T.Text -> T.Text -> InitM g s m MongoContainer
initMongo host db = do
    pipe <- liftIO $ connect (readHostPort $ T.unpack host)
    addCleanupHook $ close pipe
    return $ MongoContainer pipe master (T.toStrict db)
