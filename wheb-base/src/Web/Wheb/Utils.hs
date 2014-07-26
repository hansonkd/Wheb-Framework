{-# LANGUAGE OverloadedStrings #-}

module Web.Wheb.Utils where

import           Blaze.ByteString.Builder (Builder
                                          ,fromLazyByteString
                                          ,toLazyByteString)
import           Control.Monad
import           Data.IORef
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as TS
import           Network.HTTP.Types.Status
import           Network.Wai

import           Web.Wheb.Types

lazyTextToSBS = TS.encodeUtf8 . T.toStrict
sbsToLazyText = T.fromStrict . TS.decodeUtf8
builderToText = T.decodeUtf8 . toLazyByteString

-- | Show and pack into 'Text'
spack :: Show a => a -> T.Text
spack = T.pack . show

-- | See a 'HandlerResponse's as 'Text'
showResponseBody :: HandlerResponse -> IO T.Text
showResponseBody (HandlerResponse s r) = do
  let (_, _, f) = responseToStream $ toResponse s [] r
  f $ \streamingBody -> do
    builderRef <- newIORef mempty
    let add :: Builder -> IO ()
        add b = atomicModifyIORef builderRef $ \builder ->
            (builder `mappend` b, ())
        flush :: IO ()
        flush = return ()
    streamingBody add flush
    fmap (T.decodeUtf8 . toLazyByteString) $ readIORef builderRef

----------------------- Instances ------------------------
instance WhebContent Builder where
  toResponse = responseBuilder

instance WhebContent T.Text where
  toResponse s hds = responseBuilder s hds . fromLazyByteString . T.encodeUtf8

instance WhebContent WhebFile where
  toResponse s hds (WhebFile fp) = responseFile s hds (show fp) Nothing

----------------------- Some defaults -----------------------
defaultErr :: Monad m => WhebError -> WhebHandlerT g s m
defaultErr err = return $ HandlerResponse status500 $ 
            ("<h1>Error: " <> (T.pack $ show err) <> ".</h1>")

uhOh :: Response
uhOh = responseLBS status500 [("Content-Type", "text/html")]
      "Something went wrong on the server."