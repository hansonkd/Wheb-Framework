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
  , throwRedirect
  
  -- *** Setting a header
  , setHeader
  , setRawHeader
  
  -- * Cookies
  , setCookie
  , setCookie'
  , getCookie
  , getCookies
  , removeCookie

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
import Web.Wheb.InitM 
import Web.Wheb.Routes
import Web.Wheb.Types 
import Web.Wheb.Utils
import Web.Wheb.WhebT 
import Web.Wheb.Cookie