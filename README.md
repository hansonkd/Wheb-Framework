# Wheb

The Frictionless Haskell WAI Framework

### About


Wheb's a framework for building robust, high-concurrency web applications simply and effectively. Its primary goal is to extend the functionality of the base WAI library and to provide an easy entry point into Haskell web servers. The only prerequisite is "Learn you a Haskell" or another introductory Haskell course.

### Features

* The core datatype will let you build anything from a read-only server to a fully interactive web application with basic Haskell.
* Minimal boilerplate to start your application.
* Named routes and URL generation (though it was a trade-off between named and type-safe urls).
* Easy to use for REST APIs
* Fully database and template agnostic
* Easy handler debugging.
* Middleware
* Fast. It deploys on warp.
* Template agnostic templating. Swap out template backends on the fly.

### Plugins

Wheb makes it easy to write plugins. Plugins can add routes, middleware, settings and even handle resource cleanup on server shutdown. Named routes allow plugins to dynamically generate their routes at runtime based on settings. 

With Wheb's templating system, plugins can add default handlers that you can then override later.

Examples of plugins:

* Sessions
* Auth
* [Wheb-Mongo](http://hackage.haskell.org/package/wheb-mongo)
* [Wheb-Hastache](http://hackage.haskell.org/package/wheb-hastache)

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

Now our handler needs a route. We assign routes inside the `InitM` monad which builds our options. Wheb uses named routes and has some convenience functions for adding them to match their HTTP methods: `addGET`, `addPOST`, `addPUT` and `addDELETE`. `rootPat` is a `UrlPat` that matches on the root directory, `/`.

``` haskell
opts <- genMinOpts $ do
            addGET "home" rootPat handleHome
```

Run the server and run cURL to test...

```
$ curl http://localhost:3000/
Hello World!
```
### Templates

Wheb uses template agnostic templating. What does this mean? Each template consists of one thing: a function that takes a Typeable data and outputs a Bytestring Builder in IO context. With this, you can build your own type safe templates and use whatever you want, Blaze, Heist, Hamlet, HSP or the easy to use prebuilt Hastache plugin.

By generalizing the template rendering function in our handlers, plugins can easily render HTML pages with knowledge that generic function to render it exists. It also makes us strictly seperate our templates from the rest of our code.

Blaze is pretty popular, so lets try writing a blaze template. First lets start completely over with some new imports and our template.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy (unpack)
import           Text.Read (readMaybe)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import           Web.Wheb

homeTemplate :: WhebTemplate
homeTemplate = WhebTemplate func
  where func _ = return $ Right $ renderHtmlBuilder $ do
          H.docTypeHtml $ do
                H.body $ do
                   H.h1 "Wheb tutorial"

```
Now our rendering function is compartmentalized away from our views.

```haskell       
handleHome :: MinHandler
handleHome = renderTemplate "home" emptyContext
                   
main :: IO ()
main = do
  opts <- genMinOpts $ do
            addGET "." rootPat handleHome
            addTemplate "home" homeTemplate
  runWhebServer opts
  
```

A Wheb template takes a TemplateContext and returns either an error or a Builder, so we should properly handle any errors, but so far our home template is static so not much to worry about.

### Capturing data

Returning static data isn't too interesting, so lets add a new handler and route that echo something back to us. We are going to have to import the Typeable Library becuase we will want to cast our template context back into the correct type (Text).


``` haskell

import           Data.Typeable (cast)

...

handleEcho :: MinHandler
handleEcho = do
  msg <- getRouteParam "msg"
  renderTemplate "echo" $ TemplateContext (msg :: Text)
  
numberTemplate :: WhebTemplate
numberTemplate = WhebTemplate func
  where func (TemplateContext inp) =
          case (cast inp :: Maybe Text) of
            Nothing -> return $ Left TemplateContextError
            Just e ->  return $ Right $ renderHtmlBuilder $ do
                    H.docTypeHtml $
                           H.body $ do
                              H.h1 "Echo"
                              H.h2 $ H.toMarkup e

main :: IO ()
main = do
  opts <- genMinOpts $ do
            addGET "." rootPat handleHome
            addGET "echo" ("echo" </> (grabText "msg")) handleEcho
            addTemplate "home" homeTemplate
            addTemplate "number" numberTemplate
  runWhebServer opts
```

### Named routes

So what if you wanted to generate the path back to the handleEcho handler? With named routes, it's pretty easy:

```haskell
handleShowRoute :: MinHandler
handleShowRoute = do
  url <- getRoute "echo" [("msg", MkChunk ("My Awesome message" :: T.Text))] 
  text url
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
