{-|

This module reexports Wheb modules. It should be the only thing you need to
import to get started.

@
import           Wheb
import           Data.Text.Lazy (pack)

main :: IO ()
main = do
  opts <- generateOptions $ addGET (pack \".\") rootPat $ (text (pack \"Hi!\"))
  runTerminalCommand opts
@

-}
module Wheb 
  (
  -- * Generating an app
    generateOptions
  , genMinOpts
  {-|

  -}
  -- * Running
  , runTerminalCommand
  -- ** Running Wheb Directly
  , runWhebServer
  , runRawHandler
  , runRawHandler'
  {-|

  -}
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
  -- ** Commands
  , addCommand
  , addIOCommand
  , addCommand'
  -- *** Commands Using Optparse-Applicative
  , addOptparseCommand
  , addOptparseIOCommand
  -- ** WebSockets 
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
  {-|

  -}
  -- * Request Handlers
  -- ** Access read only resources
  , getApp
  , getWithApp
  -- ** Access mutable resources
  , getHandlerState
  , putHandlerState
  , modifyHandlerState
  , modifyHandlerState'
  -- **  Creating a 'HandlerResponse'
  , html
  , text
  , file
  , builder
  , redirect
  , throwRedirect
  {-|

  -}
  -- * Headers
  , setHeader
  , setRawHeader
  {-|

  -}
  -- * Cookies
  {-|
    Cookies are stateful - you can set a cookie and retrieve that value in the same request.
  -}
  , setCookie
  , setCookie'
  , getCookie
  , getCookies
  , removeCookie
  {-|

  -}
  -- * Settings
  , getSetting
  , getSetting'
  , getSetting''
  , getSettings
  {-|

  -}
  -- * Request reading
  , getRequest
  , getRequestHeader
  , getWithRequest
  , getQueryParams
  , getPOSTParam
  , getPOSTParams
  , getRawPOST
  {-|

  -}
  -- * Routes
  -- ** Generate Route
  , getRouteParams
  , getRouteParam
  , getRoute
  , getRoute'
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
  , module Wheb.Types
  ) where


import Control.Monad.IO.Class (MonadIO(..))

import Wheb.InitM 
import Wheb.Routes
import Wheb.Types 
import Wheb.Utils
import Wheb.WhebT 
import Wheb.Cookie
import Wheb.Commands
