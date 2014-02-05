Wheb
======

The easy Haskell WAI Framework

Objective
---------

The primary goal of the Wheb framework is to extend the functionality of the base WAI library as well as provide an easy entry point into Haskell web servers. Other servers such as Snap and Yesod make use of a number of extensions and Template Haskell. While TH is powerful in allowing you to build compile time type safe urls, it is another hurdle for someone starting out in Haskell to learn before they can get started. While Yesod and others have non-TH versions of their libraries, it just adds fragmentation within the documenation and tutorials about how to effectively use it.

The simplicity of [Scotty](http://hackage.haskell.org/package/scotty) inspired me, so I built the Wheb framework with the explicit goal that Template Haskell not be included in any part of the core server. 

Other libraries feature transformers to roll your own Reader and State based applicaiton Monads, but it would be nice if they were built in. Practically every server will have a global read-only context that shares resources between threads and a request state that can change during request processing. Having these resources built in allows for plugins that can always expect those resources to be there.

Features
--------

Currently Wheb is still very early in development. I have included some features that I hope will cover most use cases.


#### Easy Setup.
Here is a Crunchy server:

```haskell
import           Web.Wheb
import           Data.Text.Lazy (pack)

main :: IO ()
main = do
  opts <- generateOptions $ addGET (pack ".") rootPat $ (text (pack "Hi!"))
  runWhebServer (opts :: MinOpts)
```

Route handlers can be simple:

```haskell
handleSimple :: T.Text -> WhebHandler GlobalApp RequestState
handleSimple t = html $ "<h1>" <> t <> "</h1>"
```

Or add some complexity...

```haskell
homePage :: WhebHandler GlobalApp RequestState
homePage = do
  -- | Keep track of sessions...
  v <- getSessionValue "has-visted"
  setSessionValue "has-visted" "True"
  case v of
    Just _  -> do
        url  <- getRoute "blog_txt" [("slug", MkChunk ("hey" :: T.Text))]
        html $ "<h1>Welcome back!</h1><a href=\"" <> url <> "\">Go to blog</a>"
    Nothing -> do
        url  <- getRoute "faq" []
        html $ "<h1>Hello Stranger!</h1><a href=\"" <> url <> "\">FAQ</a>"
```

As you scale your code base, the core simplicity remains.

```haskell
main :: IO ()
main = do
  opts <- generateOptions $ do
      -- | Add standard WAI middlware
      addWAIMiddleware logStdoutDev
      
      -- | Add Auth middlware for current user.
      addCrunchyMiddleware authMiddleware
      
      -- | Add your application routes...
      addGET "root" rootPat homePage
      addGET "faq" "faq" $  handleSimple "FAQ"
      addPOST "post_store" ("post" </> "store") handlePOST
      
      -- | Auth Handlers.
      addGET  "current"  "current"  handleCurrentUser
      addPOST "register" "register" handleRegister
      addPOST "login"    "login"    handleLogin
      
      -- | Overloaded URLs
      addGET "blog_int"  ("blog" </> (grabInt "pk")) $ handleSimple "Number"
      addGET  "blog_txt" ("blog" </> (grabText "slug")) $ 
            (getRouteParam "slug") >>= (handleSimple . fromJust)
            
      -- | Add sub-init script that is in a sub-app to keep things tidy
      addBlogPaths
      
      -- | Initialize any backends.
      sess <- initSessionMemory
      auth <- initAuthMemory
      
      -- | Return your new global context.
      return (GlobalApp sess auth)
      
  runCrunchyServer opts
```


#### URLs
Crunchy uses named dynamically typed URLs. While this means you won't get compile-time checking of your URLs, it gives you some form of type safety beyond simple text. 

```haskell
-- | This URL will match /blog/1 but not /blog/foo
url = compilePat ("blog" </> (grabInt "pk"))
-- | Output will be Right "/blog/3/"
generateUrl url [("pk", MkChunk 3)]
-- | Output will be Left (ParamTypeMismatch "pk")
generateUrl url [("pk", MkChunk 'A')]
```

Also, because they are named you can generate one of your URLs based on its name and parameters.

```haskell
url <- getRoute "blog_txt" [("slug", MkChunk ("hey" :: T.Text))]
```

#### Middlewares
Crunchy supports WAI and its own CrunchyMiddlwares. CrunchyMiddlwares allow you to change the state before it reaches your handler. It also allows you to return a response to intercept requests.

The included auth middlware makes use of the ability to change state to set the current user before each Handler.

#### Debugging

You can run handlers and debug directly without a server:

```haskell
main :: IO ()
main = do
  opts <- generateOptions $ do
      addWhebMiddleware authMiddleware
      addGET "blog_int"  ("blog" </> (grabInt "pk")) $ handleSimple "Number"
      sess <- initSessionMemory
      auth <- initAuthMemory
      return (GlobalApp sess auth)
  
  -- | Ability to easily run your handlers w/o a server.
  hResult <- debugHandlerIO opts $ handleSimple "Hello from console!"
  either print (\r -> (showResponseBody r) >>= print) hResult
  
  -- | Or simply debug some stuff.
  debugHandlerIO opts $ do
    liftIO $ putStrLn "Testing..."
    liftIO $ putStrLn "\n\nRoutes..."
    (liftIO . print) =<< getRoute' "blog_int" [("pk", MkChunk (3 :: Int))]
    
    liftIO $ putStrLn "\n\nUsers auth..."
    (liftIO . print) =<< getCurrentUser
    (liftIO . print) =<< register "Joe" "123"
    (liftIO . print) =<< login "Joe" "123"
    (liftIO . print) =<< getCurrentUser
```

#### Plugins
There are 2 proof-of-concept plugins, Auth and Sessions. Both are implemented to be abstract interfaces for different backends. Included is a Memory backend that destroys values on server shutdown. Other backends to allow data persistence can be easily added.


#### Speed
When Wheb is deployed, it uses warp. This means you get great performance right away with almost zero configuration.

These benchmarks serving a 25kb index page (in examples/resources) were taken on a base configuration linode server...

Wheb results:

```
kyle@localhost:~$ ab -c 500 -n 10000 http://127.0.0.1:8080/

Server Software:        Warp/2.0.2
Server Hostname:        127.0.0.1
Server Port:            8080

Document Path:          /
Document Length:        23348 bytes

Concurrency Level:      500
Time taken for tests:   1.668 seconds
Complete requests:      10000
Failed requests:        0
Write errors:           0
Total transferred:      234940000 bytes
HTML transferred:       233480000 bytes
Requests per second:    5995.65 [#/sec] (mean)
Time per request:       83.394 [ms] (mean)
Time per request:       0.167 [ms] (mean, across all concurrent requests)
Transfer rate:          137560.34 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0   24  96.6     14    1015
Processing:    10   43  11.6     42     235
Waiting:        3   21   9.5     20     224
Total:         16   66  97.8     58    1065

Percentage of the requests served within a certain time (ms)
  50%     58
  66%     62
  75%     65
  80%     67
  90%     73
  95%     80
  98%     95
  99%    103
 100%   1065 (longest request)

```

And Nginx serving the same file...

```
kyle@localhost:~$ ab -c 500 -n 10000 http://127.0.0.1:80/

Server Software:        nginx/1.4.1
Server Hostname:        127.0.0.1
Server Port:            80

Document Path:          /
Document Length:        23348 bytes

Concurrency Level:      500
Time taken for tests:   1.600 seconds
Complete requests:      10000
Failed requests:        0
Write errors:           0
Total transferred:      235920000 bytes
HTML transferred:       233480000 bytes
Requests per second:    6248.32 [#/sec] (mean)
Time per request:       80.022 [ms] (mean)
Time per request:       0.160 [ms] (mean, across all concurrent requests)
Transfer rate:          143955.36 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        4   23   4.7     22      41
Processing:    17   56   9.9     56      99
Waiting:        2   22   7.0     21      54
Total:         35   78   9.4     78     131

Percentage of the requests served within a certain time (ms)
  50%     78
  66%     81
  75%     83
  80%     84
  90%     90
  95%     93
  98%     96
  99%    100
 100%    131 (longest request)
```