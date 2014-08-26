{-# LANGUAGE OverloadedStrings #-}

import Web.Wheb
import Web.Wheb.Plugins.Redis

data MyCtx = MyCtx RedisContainer

instance RedisApp MyCtx where
  getRedisContainer (MyCtx rc) = rc

main :: IO ()
main = do
  opts <- generateOptions $ do
      r <- initRedis defaultConnectInfo
      addGET "home" rootPat ((runRedis $ get "hello") >>= (text . spack))
      return (MyCtx r, ())
  
  runRawHandler opts $ do
      runRedis $ set "hello" "world"
  
  runWhebServer opts