module Wheb.Commands   (
  -- * Running
    runTerminalCommand

  -- * Commands
  , addCommand
  , addIOCommand
  , addCommand'

  -- * Commands Using Optparse-Applicative
  , addOptparseCommand
  , addOptparseIOCommand
  ) where

import           Data.Monoid ((<>))
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Writer (MonadWriter(tell), 
                                       Monoid(mempty))
import           Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import           Options.Applicative (handleParseResult, ParserInfo(..), prefs, idm)
import           Options.Applicative.Extra (execParserPure)
import           System.Environment (getArgs, getProgName)

import           Wheb.Types (Command(..), InitM(..), InitOptions(..), 
                             WhebT(..), WhebOptions(..))
import           Wheb.WhebT (runRawHandler)

-- | Add a command to be run inside of the `WhebT` monad.
addCommand' :: Command g s m -> InitM g s m ()
addCommand' cmd = InitM $ tell $ mempty { initCommands = [cmd] }

-- | Add a command to be run inside of the `WhebT` monad.
addCommand :: (Functor m, MonadIO m) => T.Text -> ([T.Text] -> WhebT g s m ()) -> InitM g s m ()
addCommand name fn = addCommand' $ Command name (\args opts -> void $ runRawHandler opts $ fn args)

-- | Add a command to be run inside of the `IO` monad.
addIOCommand :: (Functor m, MonadIO m) => T.Text -> ([T.Text] -> IO ()) -> InitM g s m ()
addIOCommand name fn = addCommand' $ Command name (\args _ -> fn args)

-- | Add a command to be processed by `optparse-applicative` and run inside `WhebT`
addOptparseCommand :: (Functor m, MonadIO m) => T.Text -> ParserInfo a -> (a -> WhebT g s m ()) -> InitM g s m ()
addOptparseCommand name parse fn = addCommand' $ Command name cmd
    where cmd args opts = do
            r <- handleParseResult $ execParserPure (prefs idm) parse (map T.unpack args)
            void $ runRawHandler opts $ fn r

-- | Add a command to be processed by `optparse-applicative` and run inside `IO`
addOptparseIOCommand :: (Functor m, MonadIO m) => T.Text -> ParserInfo a -> (a -> IO ()) -> InitM g s m ()
addOptparseIOCommand name parse fn = addIOCommand name cmd
    where cmd args = do
            r <- liftIO $ handleParseResult $ execParserPure (prefs idm) parse (map T.unpack args)
            fn r

findCommand :: T.Text -> [Command g s m] -> Maybe (Command g s m)
findCommand name = find (\(Command cmdname _) -> name == cmdname)

-- | Run a command using arguments from the terminal
runTerminalCommand :: WhebOptions g s m -> IO ()
runTerminalCommand opts = do
    fullargs <- getArgs
    case fullargs of
        [] -> printCommands
        (cmd:args) -> maybe (commandNotFound cmd) (handleCommand args) $ findCommand (T.pack cmd) (commands opts)
    where handleCommand args (Command _ fn) = fn (map T.pack args) opts
          commandNotFound cmd = do
                putStr $ cmd <> " is not a valid command"
                printCommands
          printCommands = do
                putStr "\n"
                prog <- getProgName
                let lineText = "Available Commands for " ++ prog
                putStrLn lineText
                putStrLn $ replicate (length lineText) '-'
                mapM_ (\(Command cmdname _) -> T.putStrLn $ (T.pack " - ") <> cmdname) (commands opts)
