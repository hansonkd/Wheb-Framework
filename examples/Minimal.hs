import           Web.Crunchy
import           Data.Text.Lazy (pack)

main :: IO ()
main = do
  opts <- generateOptions $ addGET (pack ".") rootPat $ (text (pack "Hi!"))
  runCrunchyServer (opts :: MinOpts)