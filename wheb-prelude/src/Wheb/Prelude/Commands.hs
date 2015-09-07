module Wheb.Prelude.Commands where

-- | Add all available Wheb.Prelude configurations. Automatically called at the
-- beginning of 'generateOpts'
addPreludeCommands :: InitM g s m ()
addPreludeCommands = do
    addCommand' runServerCommand
f