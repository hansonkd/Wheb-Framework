{-# LANGUAGE OverloadedStrings #-}

module Wheb.Utils where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Blaze.ByteString.Builder (Builder, fromLazyByteString, toLazyByteString, toByteString)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Data.Monoid ((<>), Monoid(mappend, mempty))
import qualified Data.Text.Encoding as TS (decodeUtf8, encodeUtf8)
import qualified Data.Text as TS (pack, unpack, Text)
import qualified Data.Text.Lazy as T (fromStrict, pack, Text, toStrict)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Status (status500)
import Network.Wai (Response, responseBuilder, responseFile, responseLBS, responseToStream)
import Wheb.Types (HandlerResponse(..), WhebContent(..), WhebError(..), WhebFile(..), WhebHandlerT, WhebT)
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)

lazyTextToSBS = TS.encodeUtf8 . T.toStrict
sbsToLazyText = T.fromStrict . TS.decodeUtf8
builderToText = T.decodeUtf8 . toLazyByteString
builderToStrictText = TS.decodeUtf8 . toByteString

-- | Show and pack into Lazy 'Text'
spack :: Show a => a -> T.Text
spack = T.pack . show

-- | Show and pack into Strict 'Text'
spacks :: Show a => a -> TS.Text
spacks = TS.pack . show

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

makeUUID :: (MonadIO m) => WhebT g s m TS.Text
makeUUID = liftM (TS.decodeUtf8 . toASCIIBytes) (liftIO nextRandom)

----------------------- Instances ------------------------
instance WhebContent Builder where
  toResponse = responseBuilder

instance WhebContent T.Text where
  toResponse s hds = responseBuilder s hds . fromLazyByteString . T.encodeUtf8

instance WhebContent WhebFile where
  toResponse s hds (WhebFile fp) = responseFile s hds (TS.unpack fp) Nothing

----------------------- Some defaults -----------------------
defaultErr :: Monad m => WhebError -> WhebHandlerT g s m
defaultErr (ErrorStatus s t) = return $ HandlerResponse s t
defaultErr err = return $ HandlerResponse status500 $ 
            ("<h1>Error: " <> (T.pack $ show err) <> ".</h1>")

uhOh :: Response
uhOh = responseLBS status500 [("Content-Type", "text/html")]
      "Something went wrong on the server."
