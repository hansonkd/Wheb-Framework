module Web.Crunchy.Routes where
  
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Read
import           Data.Typeable
import           Network.HTTP.Types.Method

import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))

import           Web.Crunchy.Types

getParam :: Typeable a => T.Text -> RouteParamList -> Maybe a
getParam k l = (lookup k l) >>= unwrap
  where unwrap :: Typeable a => ParsedChunk -> Maybe a
        unwrap (MkChunk a) = cast a
                                        
matchUrl :: [T.Text] -> UrlParser -> Maybe RouteParamList
matchUrl url (UrlParser f _) = f url

generateUrl :: UrlParser -> RouteParamList -> Either UrlBuildError T.Text
generateUrl (UrlParser _ f) = f

findUrlMatch :: StdMethod ->
                [T.Text] ->
                [Route g s m] ->
                Maybe (CrunchyHandler g s m, RouteParamList)
findUrlMatch _ _ [] = Nothing
findUrlMatch rmtd path ((Route _ methodMatch (UrlParser f _) h):rs) 
      | not (methodMatch rmtd) =  findUrlMatch rmtd path rs
      | otherwise = case f path of
                        Just params -> Just (h, params)
                        Nothing -> findUrlMatch rmtd path rs

--------- Url Patterns ------------
-- | Constructors to use w/o OverloadedStrings
pT :: T.Text -> UrlPat
pT = Chunk

pS :: String -> UrlPat
pS = pT . T.pack

-- | Represents root (/)
rootPat :: UrlPat
rootPat = Composed [] 

grabText :: T.Text -> UrlPat
grabText key = FuncChunk key (Just . MkChunk) TextChunk

grabInt :: T.Text -> UrlPat
grabInt key = FuncChunk key f IntChunk
  where rInt = decimal :: Reader Int
        f = ((either (const Nothing) (Just . MkChunk . fst)) . rInt)

-- | Allows for easier building of URL patterns
--   This should be the primary URL constructor.
(</>) :: UrlPat -> UrlPat -> UrlPat
(Composed a) </> (Composed b) = Composed (a ++ b)
a </> (Composed b) = Composed (a:b)
(Composed a) </> b = Composed (a ++ [b])
a </> b = Composed [a, b]
    
-- | Implementation for a UrlParser using pseudo-typed URL composition.
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
          build acc ((FuncChunk k f t):cs) = 
              case (showParam t k params) of
                      (Right  v)  -> build (acc <> [v]) cs
                      (Left err)  -> Left err
          slash = (T.pack "/")
          addSlashes list = slash <> (T.intercalate slash list) <> slash

compilePat :: UrlPat -> UrlParser
compilePat (Composed a) = UrlParser (matchPat a) (buildPat a)
compilePat a = UrlParser (matchPat [a]) (buildPat [a])

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