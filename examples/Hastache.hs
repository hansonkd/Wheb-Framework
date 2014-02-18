{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import           Data.Data 
import           Data.Generics 
import           Data.Text.Lazy (Text)
import           Web.Wheb
import           Web.Wheb.Plugins.Hastache

data Info = Info 
  { name    :: String
  , unread  :: Int } deriving (Data, Typeable)

-- You can render template varaibles either by passing a List
handleHome :: MinHandler
handleHome = renderTemplate "index" tc
  where tc = TemplateContext ([ ("name", "jacob")
                              , ("unread", "500")] :: [(Text, Text)])

-- Or by using Data.Data
handleGeneric :: MinHandler
handleGeneric = renderTemplate "index" tc
  where tc = TemplateContext $ Info "Generic Dan" 4000
  
main :: IO ()
main = do
  opts <- genMinOpts $ do
            addGET "home" rootPat handleHome
            addGET "generic" "generic" handleGeneric
            
            initHastache "examples/resources/templates"

  runWhebServer opts