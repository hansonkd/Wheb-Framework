module Wheb.Routes
  (
  -- * URL building
    (</>)
  , grabInt
  , grabText
  , pT
  , pS
  -- * Convenience constructors
  , patRoute
  -- * URL Patterns
  , compilePat
  , rootPat
  
  -- * Working with URLs
  , getParam
  , matchUrl
  , generateUrl
  , findUrlMatch
  , findSocketMatch
  , findSiteMatch
  -- * Utilities
  , testUrlParser
  ) where

import           Control.Applicative ((<$>))
import           Data.Monoid ((<>))
import qualified Data.Text as TS (null, pack, Text)
import qualified Data.Text.Encoding as TS (encodeUtf8)
import           Data.Text.Read (decimal, Reader)
import           Data.Typeable (cast, Typeable)
import           Network.HTTP.Types.Method (StdMethod)
import           Network.HTTP.Types.URI (decodePathSegments, encodePathSegments)
import           Web.Routes (runSite)
import           Wheb.Types
import           Wheb.Utils (builderToText, lazyTextToSBS, spacks, builderToStrictText)

-- | Build a 'Route' from a 'UrlPat'
patRoute :: Maybe TS.Text ->
            StdMethod -> 
            UrlPat -> 
            WhebHandlerT g s m -> 
            RouteHandler g s m
patRoute n m p = RouteHandler (Route n (==m) (compilePat p))

-- | Convert a 'UrlPat' to a 'UrlParser'
compilePat :: UrlPat -> UrlParser
compilePat (Composed a) = UrlParser (matchPat a) (buildPat a)
compilePat a = UrlParser (matchPat [a]) (buildPat [a])

-- | Represents root path @/@
rootPat :: UrlPat
rootPat = Chunk $ TS.pack ""

-- | Allows for easier building of URL patterns
--   This should be the primary URL constructor.
--   
-- > (\"blog\" '</>' ('grabInt' \"pk\") '</>' \"edit\" '</>' ('grabText' \"verb\"))
--  
(</>) :: UrlPat -> UrlPat -> UrlPat
(Composed a) </> (Composed b) = Composed (a ++ b)
a </> (Composed b) = Composed (a:b)
(Composed a) </> b = Composed (a ++ [b])
a </> b = Composed [a, b]

-- | Parses URL parameter and matches on 'Int'
grabInt :: TS.Text -> UrlPat
grabInt key = FuncChunk key f IntChunk
  where rInt = decimal :: Reader Int
        f = either (const Nothing) (Just . MkChunk . fst) . rInt

-- | Parses URL parameter and matches on 'Text'
grabText :: TS.Text -> UrlPat
grabText key = FuncChunk key (Just . MkChunk) TextChunk

-- | Constructors to use w/o OverloadedStrings
pT :: TS.Text -> UrlPat
pT = Chunk

pS :: String -> UrlPat
pS = pT . TS.pack

-- | Lookup and cast a URL parameter to its expected type if it exists.
getParam :: Typeable a => TS.Text -> RouteParamList -> Maybe a
getParam k l = lookup k l >>= unwrap
  where unwrap :: Typeable a => ParsedChunk -> Maybe a
        unwrap (MkChunk a) = cast a

-- | Convert URL chunks (split on /)                                
matchUrl :: [TS.Text] -> UrlParser -> Maybe RouteParamList
matchUrl url (UrlParser f _) = f url

-- | Runs a 'UrlParser' with 'RouteParamList' to a URL path
generateUrl :: UrlParser -> RouteParamList -> Either UrlBuildError TS.Text
generateUrl (UrlParser _ f) = f

-- | Sort through a list of routes to find a Handler and 'RouteParamList'
findUrlMatch :: StdMethod ->
                [TS.Text] ->
                [RouteHandler g s m] ->
                Maybe (WhebHandlerT g s m, RouteParamList)
findUrlMatch _ _ [] = Nothing
findUrlMatch rmtd path (RouteHandler (Route _ methodMatch (UrlParser f _)) h:rs)
      | not (methodMatch rmtd) =  findUrlMatch rmtd path rs
      | otherwise = case f path of
                        Just params -> Just (h, params)
                        Nothing -> findUrlMatch rmtd path rs

-- | Sort through socket routes to find a match
findSocketMatch :: [TS.Text] -> [SocketRoute g s m] -> Maybe (WhebSocket g s m, RouteParamList)
findSocketMatch _ [] = Nothing
findSocketMatch path (SocketRoute (UrlParser f _) h:rs) =
    case f path of
        Just params -> Just (h, params)
        Nothing -> findSocketMatch path rs

findSiteMatch :: [PackedSite g s m] -> 
                 [TS.Text] -> 
                 Maybe (WhebHandlerT g s m)
findSiteMatch [] _ = Nothing
findSiteMatch (PackedSite t site:sites) cs =
  either (const (findSiteMatch sites cs)) Just $
        runSite t site cs

-- | Test a parser to make sure it can match what it produces and vice versa
testUrlParser :: UrlParser -> RouteParamList -> Bool
testUrlParser up rpl = 
  case generateUrl up rpl of
      Left _ -> False
      Right t -> case matchUrl (decodeUrl t) up of
          Just params -> either (const False) (==t) (generateUrl up params)
          Nothing -> False
  where decodeUrl = decodePathSegments . TS.encodeUtf8

-- | Implementation for a 'UrlParser' using pseudo-typed URL composition.
--   Pattern will match path when the pattern is as long as the path, matching
--   on a trailing slash. If the path is longer or shorter than the pattern, it
--   should not match.
--   Example:
--       Given a url = "blog" </> (grabInt "pk") </> "edit"
--       This will match on "/blog/1/edit" and /blog/9999/edit/"
--       But not "/blog/1/", "/blog/1", "blog/foo/edit/", "/blog/9/edit/d",
--       nor "/blog/9/edit//"
matchPat :: [UrlPat] ->  [TS.Text] -> Maybe RouteParamList
matchPat chunks [] = matchPat chunks [TS.pack ""]
matchPat chunks t  = parse t chunks []
  where parse [] [] params = Just params
        parse [] _  params = Nothing
        parse [u] [] params | TS.null u  = Just params
                               | otherwise = Nothing
        parse _      [] _ = Nothing
        parse (u:us) (Chunk c:cs) params | TS.null c  = parse (u:us) cs params
                                           | u == c    = parse us cs params
                                           | otherwise = Nothing
        parse (u:us) (FuncChunk k f _:cs) params = do
                                            val <- f u
                                            parse us cs ((k, val):params)
        parse us (Composed xs:cs) params = parse us (xs ++ cs) params

buildPat :: [UrlPat] -> RouteParamList -> Either UrlBuildError TS.Text
buildPat pats params = addSlashes <$> build [] pats
    where build acc [] = Right acc
          build acc [Chunk c] = build (acc <> [c]) []
          build acc (Chunk c:cs) | TS.null c = build acc cs
                                   | otherwise = build (acc <> [c]) cs
          build acc (Composed xs:cs)     = build acc (xs <> cs)
          build acc (FuncChunk k _ t:cs) =
              case showParam t k params of
                      (Right  v)  -> build (acc <> [v]) cs
                      (Left err)  -> Left err
          addSlashes []   = TS.pack "/"
          addSlashes list = builderToStrictText $
                              encodePathSegments list

showParam :: ChunkType -> TS.Text -> RouteParamList -> Either UrlBuildError TS.Text
showParam chunkType k l = 
    case lookup k l of
        Just (MkChunk v) -> case chunkType of
            IntChunk -> toEither $ fmap spacks (cast v :: Maybe Int)
            TextChunk -> toEither (cast v :: Maybe TS.Text)
        Nothing -> Left NoParam
    where toEither v = case v of
                          Just b  -> Right b
                          Nothing -> Left $ ParamTypeMismatch k
