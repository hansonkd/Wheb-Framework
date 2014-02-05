{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.Text.Lazy as T
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid
import           Data.Default
import           Network.Wai.Middleware.RequestLogger

import           Web.Crunchy
import           Web.Crunchy.Utils (showResponseBody)

import           Web.Crunchy.Plugins.Auth
import           Web.Crunchy.Plugins.Session

import           Web.Crunchy.Plugins.Debug.MemoryBackend

data GlobalApp = GlobalApp { sessContainer :: SessionContainer
                           , authContainer :: AuthContainer }

data RequestState = RequestState { curUser :: Maybe AuthUser }

-- | Let our plugins know where their data is
instance SessionApp GlobalApp where
  getSessionContainer = sessContainer

instance AuthApp GlobalApp where
  getAuthContainer = authContainer

-- | Needed for Auth middleware
instance AuthState RequestState where
  getAuthUser = curUser
  modifyAuthUser f c = c { curUser = f (curUser c) }
  
instance Default RequestState where
    def = RequestState Nothing

homePage :: CrunchyHandler GlobalApp RequestState IO
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

handleSimple :: T.Text -> CrunchyHandler GlobalApp RequestState IO
handleSimple t = html $ "<h1>" <> t <> "</h1>"

handlePOST :: CrunchyHandler GlobalApp RequestState IO
handlePOST = do
    params <- getPOSTParams
    let keys   = (fmap fst params)
        values = (fmap snd params)
    currentVals <- mapM getSessionValue keys
    let curValsText = zipWith zipFunc keys currentVals
    forM_ params (\(k, v) -> setSessionValue k v)
    html $ "<h1>Session Values before SET...</h1>" <>  (mconcat $ curValsText)
    where zipFunc k v = "| Key: " <> k <> " Value: " <> (T.pack $ show v)

handleCurrentUser :: CrunchyHandler GlobalApp RequestState IO
handleCurrentUser = do
    curUser <- getCurrentUser
    html $ "<h1>Current User...</h1>" <> (T.pack $ show curUser)

handleRegister :: CrunchyHandler GlobalApp RequestState IO
handleRegister = do
    params <- getPOSTParams
    liftIO $ print params
    userName <- liftM (fromMaybe "") $ getPostParam "username"
    userPass <- liftM (fromMaybe "") $ getPostParam "password"
    params <- getPOSTParams
    liftIO $ print params
    result <- register userName userPass
    html $ "<h1>Register result...</h1>" <> (T.pack $ show result)

handleLogin :: CrunchyHandler GlobalApp RequestState IO
handleLogin = do
    userName <- liftM (fromMaybe "") $ getPostParam "username"
    userPass <- liftM (fromMaybe "") $ getPostParam "password"
    result  <- login userName userPass
    html $ "<h1>Login result...</h1>" <> (T.pack $ show result)
    
interceptMw :: CrunchyMiddleware GlobalApp RequestState IO
interceptMw = liftM Just $ html "<h1>Intercept 1</h1>"

interceptMw2 :: CrunchyMiddleware GlobalApp RequestState IO
interceptMw2 = liftM Just $ html "<h1>Intercept 2</h1>"

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
      
      -- | Initialize any backends.
      sess <- initSessionMemory
      auth <- initAuthMemory
      
      -- | Return your new global context.
      return (GlobalApp sess auth)
  
  -- | Ability to easily run your handlers w/o a server.
  hResult <- debugHandlerIO opts $ handleSimple "Hello from console!"
  either print (\r -> (showResponseBody r) >>= print) hResult
  
  -- | Or simply debug some stuff.
  debugHandlerIO opts $ do
    liftIO $ putStrLn "Testing..."
    liftIO $ putStrLn "\n\nRoutes..."
    (liftIO . print) =<< getRoute' "blog_int" []
    (liftIO . print) =<< getRoute' "blog_int" [("pk", MkChunk (3 :: Int))]
    (liftIO . print) =<< getRoute' "blog_int" [("pk", MkChunk ("hey" :: T.Text))]
    (liftIO . print) =<< getRoute' "blog_txt" [("slug", MkChunk ("hey" :: T.Text))]
    
    liftIO $ putStrLn "\n\nUsers auth..."
    (liftIO . print) =<< getCurrentUser
    (liftIO . print) =<< login "Joe" "123"
    (liftIO . print) =<< register "Joe" "123"
    (liftIO . print) =<< login "Joe" "123"
    (liftIO . print) =<< register "Joe" "123"
    (liftIO . print) =<< getCurrentUser
      
  -- | Or run a high speed warp server.
  runCrunchyServer opts