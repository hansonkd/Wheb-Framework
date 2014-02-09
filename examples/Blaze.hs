{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Read (decimal)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import           Web.Wheb


blazeResp :: H.Html -> MinHandler
blazeResp = builder "text/html" . renderHtmlBuilder

blazeForm :: H.Html
blazeForm = do
  H.p "Input a number"
  H.form H.! A.action "." H.! A.method "post" $ do
    H.input H.! A.type_ "text" H.! A.name "num" 

handleHome :: MinHandler
handleHome = do
  blazeResp $ H.docTypeHtml $ do
       H.head $ do
           H.title "Wheb numbers."
       H.body $ do
          H.h1 "Welcome to the Wheb Blaze HTML example!"
          blazeForm

handlePOST :: MinHandler
handlePOST = do
  n <- getPOSTParam "num"
  let num = n >>= (\n -> either (const Nothing) (Just . fst) (decimal n)) :: Maybe Int
  blazeResp $ H.docTypeHtml $ do
       H.head $ do
           H.title "Wheb numbers."
       H.body $ do
          H.h1 "Got some POST data!"
          case num of
              Just i -> do
                  H.p "A list of natural numbers:"
                  H.ul $ forM_ [1 .. i] (H.li . H.toHtml)
              Nothing -> H.p "Enter a valid number."
          blazeForm

main :: IO ()
main = do
  opts <- genMinOpts $ do
            addGET "." rootPat handleHome
            addPOST "numbers" rootPat handlePOST
  runWhebServer opts