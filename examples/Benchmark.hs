import           Web.Wheb
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
  opts <- generateOptions $ do
    addGET (T.pack ".") rootPat $ 
        file "examples/resources/index.html" (T.pack "text/html")
  runWhebServer (opts :: MinOpts)