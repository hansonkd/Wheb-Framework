module Web.Wheb.Routes
  (
  -- * Convenience constructors
    patRoute
  -- * URL Patterns
  , compilePat
  , rootPat
  
  -- ** URL building
  , (</>)
  , grabInt
  , grabText
  , pT
  , pS
  
  -- * Working with URLs
  , getParam
  , matchUrl
  , generateUrl
  , findUrlMatch
  ) where
  
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Read
import           Data.Typeable
import           Network.HTTP.Types.Method

import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))

import           Web.Wheb.Types

-- * Convenience constructors

patRoute :: (Maybe T.Text) -> 
            StdMethod -> 
            UrlPat -> 
            WhebHandlerT g s m -> 
            Route g s m
patRoute n m p = Route n (==m) (compilePat p)

-- * URL Patterns

-- | Convert a 'UrlPat' to a 'UrlParser'
compilePat :: UrlPat -> UrlParser
compilePat (Composed a) = UrlParser (matchPat a) (buildPat a)
compilePat a = UrlParser (matchPat [a]) (buildPat [a])

-- | Represents root path @/@
rootPat :: UrlPat
rootPat = Composed [] 



-- | Allows for easier building of URL patterns
--   This should be the primary URL constructor.
--   
--  @
--        (\"blog\" '</>' ('grabInt' \"pk\") '</>' \"edit\" '</>' ('grabText' \"verb\"))
--  @
(</>) :: UrlPat -> UrlPat -> UrlPat
(Composed a) </> (Composed b) = Composed (a ++ b)
a </> (Composed b) = Composed (a:b)
(Composed a) </> b = Composed (a ++ [b])
a </> b = Composed [a, b]

-- | Parses URL parameter and matches on 'Int'
grabInt :: T.Text -> UrlPat
grabInt key = FuncChunk key f IntChunk
  where rInt = decimal :: Reader Int
        f = ((either (const Nothing) (Just . MkChunk . fst)) . rInt)

-- | Parses URL parameter and matches on 'Text'
grabText :: T.Text -> UrlPat
grabText key = FuncChunk key (Just . MkChunk) TextChunk

-- | Constructors to use w/o OverloadedStrings
pT :: T.Text -> UrlPat
pT = Chunk

pS :: String -> UrlPat
pS = pT . T.pack



-- | Lookup and cast a URL parameter to its expected type if it exists.
getParam :: Typeable a => T.Text -> RouteParamList -> Maybe a
getParam k l = (lookup k l) >>= unwrap
  where unwrap :: Typeable a => ParsedChunk -> Maybe a
        unwrap (MkChunk a) = cast a

-- | Convert URL chunks (split on /)                                
matchUrl :: [T.Text] -> UrlParser -> Maybe RouteParamList
matchUrl url (UrlParser f _) = f url

-- | Runs a 'UrlParser' with 'RouteParamList' to a URL path
generateUrl :: UrlParser -> RouteParamList -> Either UrlBuildError T.Text
generateUrl (UrlParser _ f) = f

-- | Sort through a list of routes to find a Handler and 'RouteParamList'
findUrlMatch :: StdMethod ->
                [T.Text] ->
                [Route g s m] ->
                Maybe (WhebHandlerT g s m, RouteParamList)
findUrlMatch _ _ [] = Nothing
findUrlMatch rmtd path ((Route _ methodMatch (UrlParser f _) h):rs) 
      | not (methodMatch rmtd) =  findUrlMatch rmtd path rs
      | otherwise = case f path of
                        Just params -> Just (h, params)
                        Nothing -> findUrlMatch rmtd path rs

-- | Implementation for a 'UrlParser' using pseudo-typed URL composition.
--   Pattern will match path when the pattern is as long as the path, matching
--   on a trailing slash. If the path is longer or shorter than the pattern, it
--   should not match.
--   Example:
--       Given a url = "blog" </> (grabInt "pk") </> "edit"
--       This will match on "/blog/1/edit" and /blog/9999/edit/"
--       But not "/blog/1/", "/blog/1", "blog/foo/edit/", "/blog/9/edit/d",
--       nor "/blog/9/edit//"
matchPat :: [UrlPat] ->  [T.Text] -> Maybe RouteParamList
matchPat chunks t = parse t chunks []
  where parse [] [] params = Just params
        parse [] c  params = Nothing
        parse (u:[])  [] params | T.null u  = Just params -- Match only 1 trailing slash
                                | otherwise = Nothing
        parse (u:us) [] _ = Nothing
        parse (u:us) ((Chunk c):cs) params | u == c    = parse us cs params
                                           | otherwise = Nothing
        parse (u:us) ((FuncChunk k f _):cs) params = do
                                            val <- f u
                                            parse us cs ((k, val):params)
        parse us ((Composed xs):cs) params = parse us (xs ++ cs) params

buildPat :: [UrlPat] -> RouteParamList -> Either UrlBuildError T.Text
buildPat pats params = fmap addSlashes $ build [] pats
    where build acc [] = Right acc
          build acc ((Chunk c):cs)         = build (acc <> [c]) cs
          build acc ((Composed xs):cs)     = build acc (xs <> cs)
          build acc ((FuncChunk k _ t):cs) = 
              case (showParam t k params) of
                      (Right  v)  -> build (acc <> [v]) cs
                      (Left err)  -> Left err
          slash = (T.pack "/")
          addSlashes list = slash <> (T.intercalate slash list) <> slash

showParam :: ChunkType -> T.Text -> RouteParamList -> Either UrlBuildError T.Text
showParam chunkType k l = 
    case (lookup k l) of
        Just (MkChunk v) -> case chunkType of
            IntChunk -> toEither $ fmap (T.pack . show) (cast v :: Maybe Int)
            TextChunk -> toEither (cast v :: Maybe T.Text)
        Nothing -> Left NoParam
    where toEither v = case v of
                          Just b  -> Right b
                          Nothing -> Left $ ParamTypeMismatch k