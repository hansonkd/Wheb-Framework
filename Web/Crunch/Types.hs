{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, MultiParamTypeClasses #-}
module Web.Crunch.Types where

import           Blaze.ByteString.Builder (Builder, fromLazyByteString)
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Monoid ((<>))

import           Data.Default
import           Data.Map as M
import           Data.String (IsString(..))
import qualified Data.Text.Lazy as T
import           Data.Typeable

import           Network.Wai (Request, Response, responseBuilder)
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header

import           Data.ByteString (ByteString)

class CrunchResponse a where
  toResponse :: Status -> ResponseHeaders -> a -> Response

data HandlerResponse = forall a . CrunchResponse a => HandlerResponse Status a
type EResponse = Either CrunchError Response
type CSettings = M.Map T.Text T.Text
data CrunchError = Error500 String | Error301 | Error404
  deriving (Show)

instance Error CrunchError where 
    strMsg = Error500

data HandlerData g s m = 
  HandlerData { globalCtx :: g
              , request :: Request
              , routeParams :: RouteParamList
              , globalSettings :: CrunchOptions g s m }

data InternalState s =
  InternalState { appState     :: s
                , respHeaders  :: M.Map HeaderName ByteString } 

instance Default s => Default (InternalState s) where
  def = InternalState def def

data InitOptions g s m =
  InitOptions { initRoutes :: [ Route g s m ]
              , initSettings :: CSettings }

data CrunchOptions g s m = MonadIO m => 
  CrunchOptions { appRoutes           :: [ Route g s m ]
                , runTimeSettings     :: CSettings
                , startingCtx         :: g
                , defaultErrorHandler :: CrunchError -> CrunchHandler g s m }

instance Monoid (InitOptions g s m) where
  mappend (InitOptions a1 b1) (InitOptions a2 b2) = 
      InitOptions (a1 <> a2) (b1 <> b2)
  mempty = InitOptions mempty mempty

newtype InitM g s m a = InitM { runInitM :: WriterT (InitOptions g s m) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | CrunchT g s m
--   g -> The global confirgured context (Read-only data shared between threads)
--   s -> State initailized at the start of each request using Default
--   m -> Monad we are transforming
newtype CrunchT g s m a = CrunchT 
  { runCrunchT :: ErrorT CrunchError 
                      (ReaderT (HandlerData g s m) (StateT (InternalState s) m)) a 
  } deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans (CrunchT g s) where
  lift = CrunchT . lift . lift . lift

instance (Monad m) => MonadError CrunchError (CrunchT g s m) where
    throwError = CrunchT  . throwError
    catchError (CrunchT  m) f = CrunchT  (catchError m (runCrunchT . f))

type CrunchHandler g s m = CrunchT g s m HandlerResponse

----------------- Routes -----------------

type  RouteParamList = [(T.Text, ParsedChunk)]
type  MethodMatch = StdMethod -> Bool

data ParsedChunk = forall a. (Typeable a) => MkChunk a

data UrlParser = UrlParser ([T.Text] -> Maybe RouteParamList) 

data Route g s m = Route 
  { routeName    :: (Maybe T.Text)
  , routeMethod  :: MethodMatch
  , routeParser  :: UrlParser
  , routeHandler :: (CrunchHandler g s m) }

data UrlPat = Chunk T.Text 
            | FuncChunk T.Text (T.Text -> Maybe ParsedChunk)
            | Composed [UrlPat]

instance IsString UrlPat where
  fromString = Chunk . T.pack