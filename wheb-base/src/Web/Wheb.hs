{-|

This module reexports Wheb modules. It should be the only thing you need to
import to get started.

@
import           Web.Wheb
import           Data.Text.Lazy (pack)

main :: IO ()
main = do
  opts <- generateOptions $ addGET (pack \".\") rootPat $ (text (pack \"Hi!\"))
  runWhebServer opts
@

-}
module Web.Wheb 
  (
  -- * Handlers
  -- ** ReaderT and StateT Functionality
  -- *** ReaderT
    getApp
  , getWithApp
  -- *** StateT
  , getHandlerState
  , putHandlerState
  , modifyHandlerState
  , modifyHandlerState'
  
  -- ** Dealing with responses
  -- *** Creating a 'HandlerResponse'
  , html
  , text
  , file
  , builder
  , redirect
  -- *** Setting a header
  , setHeader
  , setRawHeader
  
  -- * Settings
  , getSetting
  , getSetting'
  , getSetting''
  , getSettings
  
  -- * Routes
  , getRouteParams
  , getRouteParam
  , getRoute
  , getRoute'
  
  -- * Request reading
  , getRequest
  , getRequestHeader
  , getWithRequest
  , getQueryParams
  , getPOSTParam
  , getPOSTParams
  , getRawPOST
  
  -- ** Running Wheb
  , runWhebServer
  , runWhebServerT
  , runRawHandler
  , runRawHandlerT
  
  -- * Initialize
  -- ** Routes
  -- *** Named routes convenience functions
  , addGET
  , addPOST
  , addPUT
  , addDELETE
  -- *** Add raw routes
  , addRoute
  , addRoutes
  , catchAll
  -- ** Sockets 
  , addWhebSocket
  -- ** Sites
  , addSite
  -- ** Middlewares
  , addWAIMiddleware
  , addWhebMiddleware
  -- ** Settings
  , addSetting
  , addSetting'
  , addSettings
  , readSettingsFile
  -- ** Cleanup
  , addCleanupHook
  -- * Running
  , generateOptions
  , genMinOpts
  -- * Routes
  -- ** URL Patterns
  , compilePat
  , rootPat
  
  -- ** URL building
  , (</>)
  , grabInt
  , grabText
  , pT
  , pS
  -- * Utilities
  , spack
  , MonadIO(..)
  -- * Types
  , module Web.Wheb.Types
  ) where


import Control.Monad.IO.Class (MonadIO(..))
import Web.Wheb.InitM (addCleanupHook, addDELETE, addGET, addPOST, addPUT, addRoute, addRoutes, addWhebSocket, addSetting, addSetting', addSettings, addSite, addWAIMiddleware, addWhebMiddleware, catchAll, generateOptions, genMinOpts, readSettingsFile)
import Web.Wheb.Routes ((</>), compilePat, grabInt, grabText, pS, pT, rootPat)
import Web.Wheb.Types (ChunkType(..), CSettings, WhebSocket, EResponse, HandlerData(..), HandlerResponse(..), InitM(..), InitOptions(..), InternalState(..), MethodMatch, MinHandler, MinOpts, MinWheb, PackedSite(..), ParsedChunk(..), Route(..), RouteParamList, SettingsValue(..), UrlBuildError(..), UrlParser(..), UrlPat(..), WhebContent(..), WhebError(..), WhebFile(..), WhebHandler, WhebHandlerT, WhebMiddleware, WhebOptions(..), WhebT(..))
import Web.Wheb.Utils (spack)
import Web.Wheb.WhebT (builder, runRawHandler, runRawHandlerT, file, redirect, getApp, getHandlerState, getPOSTParam, getPOSTParams, getQueryParams, getRawPOST, getRequest, getRequestHeader, getRoute, getRoute', getRouteParam, getRouteParams, getSetting, getSetting', getSetting'', getSettings, getWithApp, getWithRequest, html, modifyHandlerState, modifyHandlerState', putHandlerState, runWhebServer, runWhebServerT, setHeader, setRawHeader, text)