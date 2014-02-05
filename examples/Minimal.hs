import           Web.Crunchy
import           Data.Text.Lazy as T

main :: IO ()
main = do
    opts <- generateOptions $ do
        addGET (T.pack "all") rootPat $
            (text (T.pack "Hey"))
    runCrunchyServer (opts :: MinOpts)