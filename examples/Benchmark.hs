import           Web.Wheb
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
main :: IO ()
main = do
  htmlContent <- T.readFile "examples/resources/index.html"
  opts <- generateOptions $ do
    addGET (T.pack ".") rootPat $ html htmlContent
  runWhebServer (opts :: MinOpts)