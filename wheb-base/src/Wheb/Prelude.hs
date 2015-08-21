module Wheb.Prelude (
    -- * Adding Prelude
      addPrelude
    -- * Individual Components
    -- ** Commands
    , runServerCommand
    ) where

import           Control.Applicative ((<$>), (<*>))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import           Options.Applicative (handleParseResult, optional, option, short, info,
                                      auto, strOption, prefs, idm, long, metavar, help,
                                      helper, fullDesc, progDesc, header)
import           Options.Applicative.Extra (execParserPure)

import           Wheb.Types
import           Wheb.WhebT (runWhebServer)
import           Wheb.Commands (addCommand')

data RunServer = 
    RunServer { serverHost :: (Maybe String)
              , serverPort :: (Maybe Int)
              } deriving (Show)

-- | Run the server with warp with options from command line.
runServerCommand :: Command g s m
runServerCommand = Command (T.pack "runserver") fn
    where fn args opts = do
                r <- handleParseResult $ 
                    execParserPure (prefs idm) parserinfo (map T.unpack args)
                runServer opts r
          portNumber = optional $ option auto
                      ( long "port"
                         <> short 'P'
                         <> metavar "PORT"
                         <> help "Port to bind to" )
          hostName = optional $ strOption
                     ( long "host"
                        <> short 'H'
                        <> metavar "HOST"
                        <> help "Host to bind to" )
          parser = RunServer <$> hostName <*> portNumber
          parserinfo = info 
                       (helper <*> parser) 
                       ( fullDesc
                         <> progDesc "Run the server."
                         <> header "runserver - run wheb with warp" )
         
          runServer opts (RunServer sh sp) = do
             let host = fromMaybe "localhost" sh
                 port = fromMaybe 3000 sp

             runWhebServer host port opts

-- | Add all available Wheb.Prelude configurations. Automatically called at the
-- beginning of 'generateOpts'
addPrelude :: InitM g s m ()
addPrelude = do
    addCommand' runServerCommand
