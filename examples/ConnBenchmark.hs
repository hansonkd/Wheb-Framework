import           Web.Wheb
import           Data.Text.Lazy (pack)
import           Control.Concurrent
import           Control.Concurrent.STM

-- | Measure simultaneous connections
main :: IO ()
main = do
  opts <- genMinOpts $ do
    addGET (pack ".") rootPat $ (text (pack $ repeat '.'))
  
  forkIO $ loop 0 (activeConnections opts)

  runWhebServer opts

  where loop maxConns tv = do
                threadDelay 100000
                print maxConns
                curConns <- atomically $ readTVar tv
                if (curConns > maxConns)
                    then loop curConns tv
                    else loop maxConns tv
