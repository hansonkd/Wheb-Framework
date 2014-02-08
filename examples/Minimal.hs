import           Web.Wheb
import           Data.Text.Lazy (pack)

main :: IO ()
main = do
  opts <- genMinOpts $ addGET (pack ".") rootPat $ (text (pack "Hi!"))
  runWhebServer opts