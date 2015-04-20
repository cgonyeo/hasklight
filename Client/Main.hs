module Main where

import GHCJS.DOM                   ( runWebGUI
                                   , webViewGetDomDocument
                                   , postGUIAsync
                                   , postGUISync
                                   )
import GHCJS.DOM.Document          ( documentGetBody
                                   , documentGetElementById
                                   )
import GHCJS.DOM.HTMLElement       ( htmlElementSetInnerHTML )
import GHCJS.DOM.Types             ( castToHTMLButtonElement
                                   , castToHTMLTableElement
                                   )
import Control.Applicative         ( (<$>) )
import Data.Text.Lazy              ( unpack
                                   )
import Language.Javascript.JSaddle ( runJSaddle_ )

import Hasklight.AnimParams
import Hasklight.Client.Actions
import Hasklight.Client.Rendering
import Hasklight.Client.Utils


main :: IO ()
main = do
    runWebGUI $ (\ webView -> do
        Just doc <- webViewGetDomDocument webView
        Just body <- documentGetBody doc

        let runjs = postGUIAsync . runJSaddle_ webView

        Just animtbl <- fmap castToHTMLTableElement
                             <$> documentGetElementById doc "availanimlist"
        Just okbtn <- fmap castToHTMLButtonElement
                             <$> documentGetElementById doc "okbtn"
        htmlElementSetInnerHTML animtbl (unpack $ renderAnimList availAnims)

        mapM_ (addAnimAction runjs doc) [0..(length availAnims - 1)]
        applyAnimAction doc runjs okbtn
      )
