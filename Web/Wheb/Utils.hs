{-# LANGUAGE OverloadedStrings #-}
module Web.Wheb.Utils where

import           Blaze.ByteString.Builder (Builder
                                          ,fromLazyByteString
                                          ,toLazyByteString)
import           Control.Monad
import           Data.Monoid
import           Data.Conduit as C
import           Data.Conduit.List (fold)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as TS
import           Network.HTTP.Types.Status
import           Network.Wai

import           Web.Wheb.Types

lazyTextToSBS = TS.encodeUtf8 . T.toStrict
sbsToLazyText = T.fromStrict . TS.decodeUtf8

showResponseBody :: HandlerResponse -> IO T.Text
showResponseBody (HandlerResponse s r) = 
    liftM (T.decodeUtf8 . toLazyByteString) builderBody
    where chunkFlatAppend m (C.Chunk more) = m `mappend` more
          chunkFlatAppend m _ = m
          builderBody = body' (C.$$ fold chunkFlatAppend mempty)
          (_, _, body') = responseToSource $ toResponse s [] r

----------------------- Instances ------------------------
instance WhebContent Builder where
  toResponse = responseBuilder

instance WhebContent T.Text where
  toResponse s hds = responseBuilder s hds . fromLazyByteString . T.encodeUtf8

----------------------- Some defaults -----------------------
defaultErr :: Monad m => WhebError -> WhebHandlerT g s m
defaultErr err = return $ HandlerResponse status500 $ 
            ("<h1>Error: " <> (T.pack $ show err) <> ".</h1>")

uhOh :: Response
uhOh = responseLBS status500 [("Content-Type", "text/html")]
      "Something went wrong on the server."