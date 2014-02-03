module Web.Crunch.Routes where
  
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Read
import           Data.Typeable
import           Network.HTTP.Types.Method

import           Data.Maybe (fromJust)

import           Web.Crunch.Types

getParam :: Typeable a => T.Text -> RouteParamList -> Maybe a
getParam k l = (lookup k l) >>= unwrap
  where unwrap :: Typeable a => ParsedChunk -> Maybe a
        unwrap (MkChunk a) = cast a

matchUrl :: [T.Text] -> UrlParser -> Maybe RouteParamList
matchUrl url (UrlParser f) = f url

findUrlMatch :: StdMethod ->
                [T.Text] ->
                [Route g s m] ->
                Maybe (CrunchHandler g s m, RouteParamList)
findUrlMatch _ _ [] = Nothing
findUrlMatch rmtd path ((Route _ methodMatch (UrlParser f) h):rs) 
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
grabText key = FuncChunk key (Just . MkChunk)

grabInt :: T.Text -> UrlPat
grabInt key = FuncChunk key f
  where rInt = decimal :: Reader Int
        f = ((either (const Nothing) (Just . MkChunk . fst)) . rInt)

-- | Allows for easier building of URL patterns
--   This should be the primary URL constructor.
(</>) :: UrlPat -> UrlPat -> UrlPat
(Composed a) </> (Composed b) = Composed (a ++ b)
a </> (Composed b) = Composed (a:b)
(Composed a) </> b = Composed (a ++ [b])
a </> b = Composed [a, b]

-- | Implementation for a UrlParser using pseudo-typed URL composition
--   Pattern will match path when the pattern is as long as the path, matching
--   on a trailing slash. If the path is longer or shorter than the pattern, it
--   should not match.
--   Example:
--       Given a url = "blog" </> (grabInt "pk") </> "edit"
--       This will match on "blog/1/edit" and "blog/9999/edit/"
--       But not "blog/1/", "blog/1", "blog/foo/edit/", "blog/9/edit/d",
--       nor "blog/9/edit//"
matchPat :: [UrlPat] ->  [T.Text] -> Maybe RouteParamList
matchPat chunks t = parse t chunks []
  where parse [] [] params = Just params
        parse [] c  params = Nothing
        parse (u:[])  [] params | T.null u  = Just params -- Match only 1 trailing slash
                                | otherwise = Nothing
        parse (u:us) [] _ = Nothing
        parse (u:us) ((Chunk c):cs) params | u == c    = parse us cs params
                                           | otherwise = Nothing
        parse (u:us) ((FuncChunk k f):cs) params = do
                                            val <- f u
                                            parse us cs ((k, val):params)
        parse us ((Composed xs):cs) params = parse us (xs ++ cs) params

compilePat :: UrlPat -> UrlParser
compilePat (Composed a) = UrlParser $ matchPat a
compilePat a = UrlParser $ matchPat [a]