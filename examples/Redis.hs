{-# LANGUAGE OverloadedStrings #-}

import Web.Wheb
import Web.Wheb.Plugins.Redis
import Web.Wheb.Plugins.Cache

data MyCtx = MyCtx RedisContainer RedisCacheContainer

instance RedisApp MyCtx where
  getRedisContainer (MyCtx rc _) = rc

instance RedisCacheApp MyCtx where
  getRedisCacheContainer (MyCtx _ rc) = rc

main :: IO ()
main = do
  opts <- generateOptions $ do
      r <- initRedis defaultConnectInfo
      c <- initRedisCache defaultConnectInfo
      addGET "home" rootPat ((runRedis $ get "hello") >>= (text . spack))
      return (MyCtx r c, ())
  
  cv <- runRawHandler opts $ do
      runRedis $ set "hello" "world"
      
      -- | Use Wheb's caching...
      setCacheValue' "in-cache" "test" 1
      getCacheValue "in-cache"

  print cv
  
  runWhebServer opts