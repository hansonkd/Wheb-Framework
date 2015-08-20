{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import qualified Data.Text.Lazy as T

import           Wheb
import           Wheb.Plugins.Mongo

data MyApp = MyApp MongoContainer
data MyRequestState = MyRequestState

instance MongoApp MyApp where
    getMongoContainer (MyApp mc) = mc

homePage :: WhebHandler MyApp MyRequestState
homePage = do
    teams <- runAction $ do
        delete (select [] "team")
        insertMany "team" [
            ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
            ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
            ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
            ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]
        rest =<< find (select [] "team") {sort = ["home.city" =: 1]}
    text $ T.intercalate " | " $ map spack teams

main :: IO ()
main = do
  opts <- generateOptions $ do
    addGET "." rootPat $ homePage
    mongo <- initMongo "127.0.0.1:27017" "master"
    return (MyApp mongo, MyRequestState)
  runWhebServer opts