{-# LANGUAGE OverloadedStrings #-}
module Site.RootPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Site.Animations

rootPage :: [AnimMetadata] -> Html
rootPage m = docTypeHtml $ do
                H.head $ do
                    H.title "ODD"
                body $ do
                    p "Hello, World!"
