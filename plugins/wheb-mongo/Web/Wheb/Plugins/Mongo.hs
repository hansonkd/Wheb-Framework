{- |
Client interface to MongoDB database management system.

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
>      mongoRes <- runAction $ do
>          delete (select [] "team")
>          insertMany "team" [
>              ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
>              ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
>              ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
>              ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]
>          rest =<< find (select [] "team") {sort = ["home.city" =: 1]}
>      case mongoRes of
>          Left err -> text $ T.pack $ show err
>          Right teams -> text $ T.intercalate " | " $ map (T.pack . show) teams
>  
>  main :: IO ()
>  main = do
>    opts <- generateOptions $ do
>      addGET "." rootPat $ homePage
>      mongo <- initMongo "127.0.0.1:27017" "master"
>      return (MyApp mongo, MyRequestState)
>    runWhebServer opts
-}

module Web.Wheb.Plugins.Mongo (
      runAction
    , initMongo
    , MongoApp (..)
    , MongoContainer
    , module Database.MongoDB
    ) where

import qualified Data.Text.Lazy as T
import           Database.MongoDB
import           Web.Wheb

data MongoContainer = MongoContainer Pipe AccessMode Database

class MongoApp a where
    getMongoContainer :: a -> MongoContainer

runAction :: (MongoApp g, MonadIO m) => 
             Action IO a -> 
             WhebT g s m (Either Failure a)
runAction action = do
    (MongoContainer pipe mode db) <- getWithApp getMongoContainer
    liftIO $ access pipe mode db action

-- | Initialize mongo with \"host:post\" and default database.
initMongo :: T.Text -> T.Text -> InitM g s m MongoContainer
initMongo host db = do
    pipe <- liftIO $ runIOE $ connect (readHostPort $ T.unpack host)
    return $ MongoContainer pipe master (T.toStrict db)