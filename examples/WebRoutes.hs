{-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings #-}

import Prelude              hiding (id, (.))
import Data.Text            (Text)
import Control.Category     (Category(id, (.)))
import Control.Monad.Trans  (MonadIO(liftIO), lift)
import Text.Boomerang.TH    (makeBoomerangs)
import Web.Routes           (Site(..), RouteT(..), decodePathInfo, encodePathInfo, runSite, showURL)
import Web.Routes.Boomerang (Router, (<>), (</>), int, parse1, boomerangSiteRouteT, anyText, parseTexts)

import Web.Wheb hiding ((</>))

-- | the routes
data Sitemap
    = Home
    | UserOverview
    | UserDetail Int
    | Article Int Text
    deriving (Eq, Show)

$(makeBoomerangs ''Sitemap)

sitemap =
   (  rHome
   <> "users" . users
   <> rArticle . ("article" </> int . "-" . anyText)
   )
 where
   users  =  rUserOverview
          <> rUserDetail </> int

handle url = case url of
  Home -> lift handleHome
  _    -> lift $ text $ spack url

site :: Site Sitemap MinHandler
site = boomerangSiteRouteT handle sitemap

handleHome :: MinHandler
handleHome = text "This is home."

main :: IO ()
main = do
  opts <- genMinOpts $ do
      addSite "/" site
  runWhebServer opts

