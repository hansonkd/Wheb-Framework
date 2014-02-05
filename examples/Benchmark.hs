import           Control.Monad
import           Web.Wheb
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TS

main :: IO ()
main = do
  htmlContent <- TS.readFile "examples/resources/index.html"
  opts <- generateOptions $ do
    addGET (T.pack ".") rootPat $ html $ T.fromStrict htmlContent
  runWhebServer (opts :: MinOpts)