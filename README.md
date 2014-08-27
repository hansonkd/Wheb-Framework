# Wheb

The Frictionless Haskell WAI Framework

### About

Wheb's a framework for building robust, high-concurrency web applications simply and effectively. Its primary goal is to extend the functionality of the base WAI library and to provide an easy entry point into Haskell web servers. The only prerequisite is "Learn you a Haskell" or another introductory Haskell course.

Wheb focuses on having a central datastructure for every part of your application. Some frameworks force you into IO for extensions like websockets where you lose the tools in your application monad. Everything in Wheb is built off of the WhebT Transformer. That means your terminal management commands, HTTP Handlers and WebSockets all have one datatype. Build a plugin once, use it everywhere.

### Features

* The core datatype will let you build anything from a read-only server to a fully interactive web application with basic Haskell.
* Minimal boilerplate to start your application.
* Session, Auth and Cache interfaces are built in. Just drop in a backend.
* Choice between type-safe web-routes or simpler pattern-based named-routes.
* Easy to use for REST APIs
* WebSockets
* Fully database and template agnostic
* Easy handler debugging.
* Middleware
* Fast. It deploys on warp.

### Plugins

Wheb makes it easy to write plugins. Plugins can add routes, middleware, settings and even handle resource cleanup on server shutdown. Named routes allow plugins to dynamically generate their routes at runtime based on settings. 

Examples of plugins:

* Sessions
* Auth
* Cache
* [Wheb-Mongo](http://hackage.haskell.org/package/wheb-mongo)
* [Wheb-Redis](http://hackage.haskell.org/package/wheb-redis)
* [Wheb-Strapped](http://hackage.haskell.org/package/wheb-strapped)

Getting Started
---------------

### Installation:

Install wheb normally with cabal or clone the git repository.

    cabal install wheb

### Program structure.

Create a new file `Main.hs` with the following contents:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import           Web.Wheb

main :: IO ()
main = do
  opts <- genMinOpts $ return ()
  runWhebServer opts
```

Here, you can see that a Wheb application is essentially two parts, generating the options and running the options in a warp server. We have turned on the OverloadedStrings pragma for convenience, but it is optional (you just need to pack the text yourself.)

Since a server with empty options isn't very useful, lets make a handler.

### Returning content

Our handler will have the type `MinHandler` which is a type synonym to `WhebHandler () () IO`. We will discuss what the longer signature means later. 

There are a couple of convenience functions to set content-type and return content, `text`, `html`, `builder` and `file`. We will use the basic `text` which sets the content-type to `text/plain`.

``` haskell
handleHome :: MinHandler
handleHome = text $ "Hello World!"
```

Now our handler needs a route. We assign routes inside the `InitM` monad which builds our options. Wheb uses named routes and has some convenience functions for adding them to match their HTTP methods: `addGET`, `addPOST`, `addPUT` and `addDELETE`. `rootPat` is a `UrlPat` that matches on the root directory, `/`. If you want to use type-safe [web-routes](http://hackage.haskell.org/package/web-routes), there is full support for that with the `addSite` function. See `WebRoutes.hs` in examples for more information.

``` haskell
opts <- genMinOpts $ do
            addGET "home" rootPat handleHome
```

Run the server and run cURL to test...

```
$ curl http://localhost:3000/
Hello World!
```
### Capturing data

Returning static data isn't too interesting, so lets add a new handler and route that echo something back to us. Lets import Monoid's `<>` to make appending easier...

``` haskell
handleEcho :: MinHandler
handleEcho = do
  msg <- getRouteParam "msg"
  text $ "Msg was: " <> msg

...
  
  opts <- genMinOpts $ do
            addGET "home" rootPat handleHome
            addGET "echo" ("echo" </> (grabText "msg")) handleEcho
```
Now test:

```
$ curl http://localhost:3000/echo/hello
Msg was: hello
```

### Named routes

So what if you wanted to generate the path back to the handleEcho handler? With named routes, it's pretty easy:

```haskell
handleHome :: MinHandler
handleHome = do
  url <- getRoute "echo" [("msg", MkChunk ("My Awesome message" :: T.Text))] 
  html $ "<html><body><a href=\"" <> url <> "\">Echo My Awesome message</a></body></html>!"
```

### Use the same context for everything

Wheb lets you use the `WhebT` context on any level using the `runRawHandler`. This can be useful for setup tasks, management commands or debugging.


### WebSockets

Wheb has built in support for WebSockets. It allows you to use the same `WhebT` Monad in both Websockets and normal HTTP.

Here is an example of a dead simple chat application using `TChan`

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.ByteString.Lazy as B
import           Web.Wheb
import           Network.WebSockets as W

data MyApp = MyApp (TChan B.ByteString)
data MyHandlerData = MyHandlerData (TChan B.ByteString)

-- | This duplicates the connection for new Users
tchanMw :: MonadIO m => WhebMiddleware MyApp MyHandlerData m
tchanMw = do
  (MyApp chan) <- getApp
  newChan <- liftIO $ atomically $ dupTChan chan
  putHandlerState (MyHandlerData newChan)
  return Nothing

readHandler :: W.Connection -> WhebT MyApp MyHandlerData IO ()
readHandler c = do
    (MyHandlerData chan) <- getHandlerState
    forever $ liftIO $ do
        msg <- atomically $ readTChan chan
        W.sendTextData c msg

writeHandler :: W.Connection -> WhebT MyApp MyHandlerData IO ()
writeHandler c = do
    (MyHandlerData chan) <- getHandlerState
    forever $ liftIO $ do
        msg <- W.receiveDataMessage c
        let bmsg = case msg of
              W.Text m -> m
              W.Binary m -> m
        atomically $ writeTChan chan bmsg

main :: IO ()
main = do
  opts <- generateOptions $ do
            addWhebMiddleware tchanMw
            startingChan <- liftIO $ newTChanIO

            addWhebSocket (rootPat </> "read") readHandler
            addWhebSocket (rootPat </> "write") writeHandler

            return $ (MyApp startingChan, MyHandlerData startingChan)

  runWhebServer opts
```

## Global contexts and Handler State.

Getting URL parameters really isn't interesting either since we aren't really changing anything, so lets add a global context. There are two main parts of a handler's signature `WhebHandlerT g s m`, `g` and `s`. `g` refers to the read-only global context that holds thread-safe resources to share between requests. `s` is the handler state that is request specific.

### Global context

Lets get started on using the global context. First, we have to define our types and swap out `genMinOpts` for `generateOptions`. `generateOptions` needs to return the global context and handler state in a tuple, which get packed inside our options.

`MyApp` is our global context and `MyState` is our handler state.

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Web.Wheb
import qualified Data.Text.Lazy as T

data MyApp = MyApp T.Text
data MyState = MyState
type MyHandler = WhebHandler MyApp MyState 

handleHome :: MyHandler
handleHome = text $ "Hello World!"

main :: IO ()
main = do
  opts <- generateOptions $ do
            addGET "home" rootPat handleHome
            return (MyApp "Tutorial App", MyState)
  runWhebServer opts
```

We can rewrite `handleHome` to display the data we used when generating options:

``` haskell
handleHome :: MyHandler
handleHome = do
  (MyApp appname) <- getApp
  text $ "Welcome to: " <> appname
```

### Handler state

We can expand `MyState` to take data. Normally, this would be most beneficial when implementing middleware in which every request would have some unique information (such as current logged in user, permissions, etc)...

``` haskell
data MyState = MyState T.Text deriving (Show)
```

At the start of each request, each handler state get state is initialized with the data from options and any changes made do not affect other requests.

Here we changed `handleHome` to be stateful.

``` haskell
handleHome :: MyHandler
handleHome = do
  state <- getHandlerState
  putHandlerState $ MyState "This changed."
  state2 <- getHandlerState
  text $ "Start state: " <> (spack state) <> ". End state: " <> (spack state2)

...

   opts <- generateOptions $ do
          addGET "home" rootPat handleHome
          return (MyApp "Tutorial App", MyState "In the beginning.")

```

### Handling changing data between requests.

Because the state changes on the handler state don't affect other requests, in order to properly handle changing data between threads, you should use the STM library. One way of doing this can be found in `Stateful.hs` example, where we count page hits.
