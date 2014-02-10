module TestRoutes where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Test.QuickCheck
import Test.QuickCheck.Gen (choose)
import Web.Wheb
import Web.Wheb.Routes (testUrlParser)

data UrlPair = UrlPair UrlPat RouteParamList
data UrlChunk = UrlChunk {unwrapChunk :: UrlPat}

generateParamName = suchThat (listOf1 $ elements ['a'..'z']) (\n -> (length n) > 5)

instance Show UrlPair where
    show (UrlPair up pl) = "UrlParser | " ++ (show up)

instance Arbitrary UrlChunk where
    arbitrary = do
        typ <- choose (0, 2) :: Gen Int
        case typ of
            0 -> generateParamName >>= (return . UrlChunk . grabInt . T.pack)
            1 -> generateParamName >>= (return . UrlChunk . grabText . T.pack)
            2 -> (generateParamName) >>= (return . UrlChunk . pS)

instance Arbitrary UrlPat where
    arbitrary = (sized vector) >>= (return . Composed . fmap unwrapChunk)

instance Arbitrary UrlPair where
    arbitrary = do
        urlPat <- arbitrary
        params <- buildParams [urlPat]
        return $ UrlPair urlPat params
     where buildParams pats = loop pats []
           loop [] acc = return acc
           loop ((Composed a):ps) acc = loop (a ++ ps) acc
           loop ((FuncChunk n _ IntChunk):ps) acc = do
                int <- suchThat arbitrary (\a -> a > 0) :: Gen Int
                loop ps ((n, MkChunk int):acc)
           loop ((FuncChunk n _ TextChunk):ps) acc = do
                text <- arbitrary :: Gen String
                loop ps ((n, MkChunk $ T.pack text):acc)
           loop ((Chunk n):ps) acc = loop ps acc

checkURL :: UrlPair -> Bool
checkURL (UrlPair up pl) = testUrlParser (compilePat up) pl
