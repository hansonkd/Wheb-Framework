Let's get Crunchy!
======

The easy Haskell WAI Framework

Objective
---------

The primary goal of the Crunchy framework is to extend the functionality of the
base WAI library as well as provide an easy entry point into Haskell web
servers. Other servers such as Snap and Yesod make use of a number of extensions
and Template Haskell. While TH is powerful in allowing you to build compile time
type safe urls, it is another hurdle for someone starting out in Haskell to learn before they can get started. While Yesod and others have non-TH versions of their libraries, it just adds fragmentation within the documenation and
tutorials about how to effectively use it.

So, I built the Crunchy framework with the explicit goal that Template Haskell
not be included in any part of the core server.

Other libraries feature transformers to roll your own Reader and State based Monads, but it would be nice if they were built in. Practically every server will have a global read-only context that shares resources between threads and a request state that can change during request processing. Having these resources built in allows for plugins that can always expect those resources to be there.

Features
--------

Currently Crunchy is still very early in development. I have included some features that I hope will cover most use cases.


#### Easy Setup.
Here is a Crunchy server:

```
main :: IO ()
main = do
  opts <- generateOptions $ do
      addGET rootPat $ html "<h1>Intercept 2</h1>"
  runCrunchyServer opts
```
#### URLs
Crunchy uses named dynamically typed URLs. While this means you won't get compile-time checking of your URLs, it gives you some form of type safety beyond simple text. Also, because they are named you can generate one of your URLs based on its name and parameters.

```
-- | This URL will match /blog/1 but not /blog/foo
url = compilePat ("blog" </> (grabInt "pk"))
-- | Output will be Right "/blog/3/"
generateUrl url [("pk", MkChunk 3)]
-- | Output will be Left (ParamTypeMismatch "pk")
generateUrl url [("pk", MkChunk 'A')]
```

#### Middlewares
Crunchy supports WAI and its own CrunchyMiddlwares. CrunchyMiddlwares allow you
to changes you want to the state before it reaches your handler. It also allows you to return a response to intercept requests for security.

The included auth middlware makes use of the ability to change state to set the current user before each Handler.

#### Plugins
There are 2 proof-of-concept plugins, Auth and Sessions. Both are implemented to be abstract interfces for different backends. Included is a Memory backend that destroys values on server shutdown. Other backends to allow data persistence can be easily added. 