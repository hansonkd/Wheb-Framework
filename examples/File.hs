import           Web.Wheb
import           Data.Text (Text, pack)

main :: IO ()
main = do
  opts <- genMinOpts $ do
    addGET (pack ".") rootPat $ 
        file (pack "examples/resources/index.html") (pack "text/html")
    addGET (pack "faq") rootPat $ 
        file (pack "examples/resources/faq.html") (pack "text/html")
  runWhebServer opts