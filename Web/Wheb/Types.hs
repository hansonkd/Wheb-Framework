{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Web.Wheb.Types where

import           Blaze.ByteString.Builder (Builder, fromLazyByteString)
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as LBS
import           Data.Default
import           Data.Map as M
import           Data.String (IsString(..))
import qualified Data.Text.Lazy as T
import           Data.Typeable

import           Network.Wai (Request, Response, Middleware, responseBuilder)
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Parse
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header

import           Data.ByteString (ByteString)

class WhebContent a where
  toResponse :: Status -> ResponseHeaders -> a -> Response

data HandlerResponse = forall a . WhebContent a => HandlerResponse Status a
data WhebFile = WhebFile FilePath
type EResponse = Either WhebError Response
data SettingsValue = forall a. (Typeable a) => MkVal a
type CSettings = M.Map T.Text SettingsValue
data WhebError = Error500 String 
                  | Error404 
                  | URLError T.Text UrlBuildError
  deriving (Show)

instance Error WhebError where 
    strMsg = Error500

data HandlerData g s m = 
  HandlerData { globalCtx      :: g
              , request        :: Request
              , postData       :: ([Param], [File LBS.ByteString])
              , routeParams    :: RouteParamList
              , globalSettings :: WhebOptions g s m }

data InternalState s =
  InternalState { reqState     :: s
                , respHeaders  :: M.Map HeaderName ByteString } 

instance Default s => Default (InternalState s) where
  def = InternalState def def

data InitOptions g s m =
  InitOptions { initRoutes      :: [ Route g s m ]
              , initSettings    :: CSettings
              , initWaiMw       :: Middleware
              , initWhebMw   :: [ WhebMiddleware g s m ] }

data WhebOptions g s m = MonadIO m => 
  WhebOptions { appRoutes           :: [ Route g s m ]
              , runTimeSettings     :: CSettings
              , warpSettings        :: Warp.Settings
              , startingCtx         :: g
              , waiStack            :: Middleware
              , whebMiddlewares     :: [ WhebMiddleware g s m ]
              , defaultErrorHandler :: WhebError -> WhebHandlerT g s m }

instance Monoid (InitOptions g s m) where
  mappend (InitOptions a1 b1 c1 d1) (InitOptions a2 b2 c2 d2) = 
      InitOptions (a1 <> a2) (b1 <> b2) (c2 . c1) (d1 <> d2)
  mempty = InitOptions mempty mempty id mempty

newtype InitM g s m a = InitM { runInitM :: WriterT (InitOptions g s m) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | WhebT g s m
--   g -> The global confirgured context (Read-only data shared between threads)
--   s -> State initailized at the start of each request using Default
--   m -> Monad we are transforming
newtype WhebT g s m a = WhebT 
  { runWhebT :: ErrorT WhebError 
                  (ReaderT (HandlerData g s m) (StateT (InternalState s) m)) a 
  } deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans (WhebT g s) where
  lift = WhebT . lift . lift . lift

instance (Monad m) => MonadError WhebError (WhebT g s m) where
    throwError = WhebT . throwError
    catchError (WhebT m) f = WhebT  (catchError m (runWhebT . f))

type MinWheb a = WhebT () () IO a
type MinOpts = WhebOptions () () IO
type WhebHandler g s      = WhebT g s IO HandlerResponse
type WhebHandlerT g s m   = WhebT g s m HandlerResponse
type WhebMiddleware g s m = WhebT g s m (Maybe HandlerResponse)
----------------- Routes -----------------

type  RouteParamList = [(T.Text, ParsedChunk)]
type  MethodMatch = StdMethod -> Bool

data ParsedChunk = forall a. (Typeable a, Show a) => MkChunk a

data UrlBuildError = NoParam | ParamTypeMismatch T.Text | UrlNameNotFound
     deriving (Show) 

-- | A Parser should be able to extract params and regenerate URL from params.
data UrlParser = UrlParser 
    { parseFunc :: ([T.Text] -> Maybe RouteParamList)
    , genFunc   :: (RouteParamList -> Either UrlBuildError T.Text) }

data Route g s m = Route 
  { routeName    :: (Maybe T.Text)
  , routeMethod  :: MethodMatch
  , routeParser  :: UrlParser
  , routeHandler :: (WhebHandlerT g s m) }

data ChunkType = IntChunk | TextChunk

data UrlPat = Chunk T.Text 
            | FuncChunk T.Text (T.Text -> Maybe ParsedChunk) ChunkType
            | Composed [UrlPat]

instance IsString UrlPat where
  fromString = Chunk . T.pack