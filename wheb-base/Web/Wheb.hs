{-|

This module reexports Wheb modules. It should be the only thing you need to
import to get started.

@
import           Web.Wheb
import           Data.Text.Lazy (pack)

main :: IO ()
main = do
  opts <- generateOptions $ addGET (pack \".\") rootPat $ (text (pack \"Hi!\"))
  runWhebServer (opts :: MinOpts)
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
  , getReqState
  , putReqState
  , modifyReqState
  , modifyReqState'
  
  -- ** Dealing with responses
  -- *** Creating a 'HandlerResponse'
  , html
  , text
  , file
  , builder
  -- *** Setting a header
  , setHeader
  , setRawHeader
  
  -- * Settings
  , getSetting
  , getSetting'
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
  , debugHandler
  , debugHandlerT
  
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
  -- ** Middlewares
  , addWAIMiddleware
  , addWhebMiddleware
  -- ** Settings
  , addSetting
  , addSetting'
  , addSettings
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


import Web.Wheb.WhebT
import Web.Wheb.InitM
import Web.Wheb.Types
import Web.Wheb.Routes
import Web.Wheb.Utils
import Control.Monad.IO.Class