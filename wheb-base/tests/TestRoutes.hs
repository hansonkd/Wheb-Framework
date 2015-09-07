module TestRoutes where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Gen (choose)
import Wheb
import Wheb.Routes (testUrlParser)

data UrlPair = UrlPair UrlPat RouteParamList

-- | Produce semi-clean strings of reasonable length to prevent collision.
genString :: Gen String
genString = suchThat (listOf $ elements ['a'..'z']) (\n -> (length n) > 5)

instance Show UrlPair where
    show (UrlPair up pl) = "UrlParser | " ++ (show up) ++ " | " ++ (show pl)

instance Arbitrary UrlPat where
    arbitrary = (sized $ flip vectorOf cs) >>= (return . Composed)
     where cs :: Gen UrlPat
           cs = oneof 
            [ genString >>= (return . grabInt . T.pack)
            , genString >>= (return . grabText . T.pack)
            , genString >>= (return . pS) ]

instance Arbitrary UrlPair where
    arbitrary = do
        urlPat <- arbitrary
        params <- buildParams [urlPat]
        return $ UrlPair urlPat params
     where buildParams pats = loop pats []
           loop [] acc = return acc
           loop ((Composed a):ps) acc = loop (a ++ ps) acc
           loop ((FuncChunk n _ IntChunk):ps) acc = do
                int <- suchThat (arbitrary) (\n -> (n) > 5) :: Gen Int
                loop ps (acc ++ [(n, MkChunk int)])
           loop ((FuncChunk n _ TextChunk):ps) acc = do
                text <- arbitrary :: Gen String
                loop ps (acc ++ [(n, MkChunk $ T.pack text)])
           loop ((Chunk n):ps) acc = loop ps acc

checkURL :: UrlPair -> Bool
checkURL (UrlPair up pl) = testUrlParser (compilePat up) pl
