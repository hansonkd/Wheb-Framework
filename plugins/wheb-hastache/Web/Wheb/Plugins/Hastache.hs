{-|
Basic Hastache integration. Recursively scans a directory for templates.

Lets say you have this template structure:

@
templates/index.ht
templates\/some_folder\/faq.hastache
@

Running @initHastache \"templates\"@ will let you access the templates with the
names \"index\" and \"some_folder\/faq\"
-}

module Web.Wheb.Plugins.Hastache 
  ( initHastache
  ) where

import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import           Data.Typeable (cast)
import           System.Directory (doesFileExist)
import           System.FilePath (combine)
import           System.FilePath.Find
import           System.FilePath.Posix (addTrailingPathSeparator)
import           Text.Hastache 
import           Text.Hastache.Context 
import           Web.Wheb

-- | Scans a directory and adds files ending with \".ht\" or \".hastache\" to 
-- our template system with the name equal to the name without extension.
initHastache :: MonadIO m => FilePath -> InitM g s m ()
initHastache fp = do
  files <- liftIO $ find always match path
  forM_ files addfile
  where addfile fn = addTemplate (tn fn) (WhebTemplate $ templateFunc fn)
        path = addTrailingPathSeparator fp
        readFunc = templateRead fp
        config = (defaultConfig :: MuConfig IO) { muTemplateRead = readFunc } 
        match = extension ==? ".hastache" ||? extension ==? ".ht"
        tn = maybe T.empty id . cleanFn . T.pack
        cleanFn fn = T.stripPrefix (T.pack path) $ head $ T.split (=='.') fn
        templateFunc fn (TemplateContext c) = do
          let ctx = case (cast c :: Maybe [(T.Text, T.Text)]) of
                  Nothing -> (mkGenericContext c)
                  Just l  -> (mkStrContext $ 
                              (\k -> maybe MuNothing MuVariable $ lookup k l))
          b <- hastacheFileBuilder config fn ctx
          return $ Right b

templateRead :: FilePath -> FilePath -> IO (Maybe BS.ByteString)
templateRead baseDir fn = do
    fe <- doesFileExist fullFileName
    if fe
        then liftM Just $ BS.readFile fullFileName
        else return Nothing
  where fullFileName = combine baseDir fn
