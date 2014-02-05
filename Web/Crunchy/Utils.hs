{-# LANGUAGE OverloadedStrings #-}
module Web.Crunchy.Utils where

import           Blaze.ByteString.Builder (Builder, fromLazyByteString)
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as TS
import           Network.HTTP.Types.Status
import           Network.Wai

import           Web.Crunchy.Types

lazyTextToSBS = TS.encodeUtf8 . T.toStrict
sbsToLazyText = T.fromStrict . TS.decodeUtf8

----------------------- Instances ------------------------
instance CrunchyContent Builder where
  toResponse = responseBuilder

instance CrunchyContent T.Text where
  toResponse s hds = responseBuilder s hds . fromLazyByteString . T.encodeUtf8


----------------------- Some defaults -----------------------
defaultErr :: Monad m => CrunchyError -> CrunchyHandler g s m
defaultErr err = return $ HandlerResponse status500 $ 
            ("<h1>Error: " <> (T.pack $ show err) <> ".</h1>")

uhOh :: Response
uhOh = responseLBS status500 [("Content-Type", "text/html")]
      "Something went wrong on the server."