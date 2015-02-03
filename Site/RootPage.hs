{-# LANGUAGE OverloadedStrings #-}
module Site.RootPage where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad

import Site.Animations

rootPage :: [AnimMetadata] -> Html
rootPage m = docTypeHtml $ do
                H.head $ do
                    H.title "ODD"
                    link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css"
                    link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap-theme.min.css"
                    link ! rel "stylesheet" ! href "/static/css/colpick.css"
                    link ! rel "stylesheet" ! href "/static/css/rootpage.css"
                    script ! src "http://code.jquery.com/jquery-2.1.3.min.js" $ ""
                    script ! src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/js/bootstrap.min.js" $ ""
                    script ! src "/static/js/numbervalidation.js" $ ""
                    script ! src "/static/js/colpick.js" $ ""
                    script ! src "/static/js/ByteBufferAB.min.js" $ ""
                    script ! src "/static/js/Long.min.js" $ ""
                    script ! src "/static/js/ProtoBuf.min.js" $ ""
                    script ! src "/static/js/rootpage.js" $ ""
                body $ do
                    nav ! class_ "navbar navbar-default" ! customAttribute "role" "navigation" $ do
                        H.div ! class_ "container-fluid" $ do
                            H.div ! class_ "navbar-header" $ do
                                a ! class_ "navbar-brand" ! href "/" $ "ODD"
                            H.div ! class_ "collapse navbar-collapse" $ do
                                ul ! class_ "nav navbar-nav" $ do
                                    li $ a ! href "#" $ "Save Preset"
                                    li $ a ! href "#" $ "Load Preset"
                                    li $ a ! href "#" $ "Manage Presets"
                                ul ! class_ "nav navbar-nav navbar-right" $ do
                                    li $ a ! href "" $ "172.16.10.153"
                                    li $ a ! href "#" $ H.span ! class_ "glyphicon glyphicon-cog" $ ""
                    H.div ! class_ "container" $ do
                        forM_ (zip [1..(length availAnims)] availAnims) (uncurry renderAvailAnim)
                        H.div ! A.id "btnrow" ! class_ "row" $ do
                            H.div ! class_ "col-xs-offset-5 col-xs-2" $
                                button ! type_ "button"
                                       ! class_ "btn btn-default"
                                       ! customAttribute "data-toggle" "modal"
                                       ! customAttribute "data-target" "#animadd"
                                       $
                                    H.span ! class_ "glyphicon glyphicon-plus" $ ""
                            H.div ! class_ "col-xs-offset-3 col-xs-2" $ do
                                button ! A.id "rfbtn" ! class_ "btn btn-default" $
                                    H.span ! class_ "glyphicon glyphicon-refresh" $ ""
                                button ! A.id "okbtn" ! class_ "btn btn-default" $
                                    H.span ! class_ "glyphicon glyphicon-ok" $ ""
                        animAddModal availAnims

animAddModal :: [AvailAnim] -> Html
animAddModal anims = 
        H.div ! A.id "animadd"
              ! class_ "modal fade"
              ! customAttribute "role" "dialog"
              ! customAttribute "aria-hidden" "true"
              $
            H.div ! class_ "modal-dialog modal-sm" $
                H.div ! class_ "modal-content" $ do
                    H.div ! class_ "modal-header" $ do
                        button ! type_ "button"
                               ! class_ "close"
                               ! customAttribute "data-dismiss" "modal"
                               ! customAttribute "aria-label" "Close"
                               $
                            H.span ! customAttribute "aria-hidden" "true" ! class_ "glyphicon glyphicon-remove" $ ""
                        h4 ! class_ "modal-title" $ "Select an animation..."
                    H.div ! class_ "modal-body" $
                        table ! class_ "table" $ 
                            forM_ (zip [1..(length anims)] anims)
                                (\(num,(AvailAnim n _)) ->
                                    tr $ td $
                                        a ! A.id (toValue $ "a-sel-" ++ show num)
                                          ! class_ "anim-selected"
                                          ! href "#" 
                                          ! customAttribute "data-dismiss" "modal"
                                          ! customAttribute "aria-label" "Close"
                                          $ toHtml n
                                )

renderAvailAnim :: Int -> AvailAnim -> Html
renderAvailAnim num (AvailAnim nam opts) =
        H.div ! A.id (toValue $ "avail-anim-" ++ show num) ! class_ "hidden" $
            H.div ! class_ "panel panel-default" $
                H.div ! class_ "panel-body" $ do
                    H.div ! class_ "row" $ do
                        H.div ! class_ "col-xs-8" $
                            h3 ! class_ "anim-name" $ toHtml nam
                        H.div ! class_ "col-xs-2" $ 
                            select ! class_ "form-control anim-bl" $ do
                                forM_ blendingOpts (\x -> option $ toHtml x)
                        H.div ! class_ "col-xs-2" $ do
                            button ! class_ "btn btn-default anim-down" $ 
                                H.span ! class_ "glyphicon glyphicon-menu-down" $ ""
                            button ! class_ "btn btn-default anim-up" $ 
                                H.span ! class_ "glyphicon glyphicon-menu-up" $ ""
                            button ! class_ "btn btn-default del-anim" $ 
                                H.span ! class_ "glyphicon glyphicon-remove" $ ""
                    hr
                    H.div ! A.id "anims-container" ! class_ "row" $ 
                        forM_ opts
                            (\opt -> H.div ! class_ "col-md-4" $ 
                                     case opt of
                                         DoubleOpt n l h -> do
                                             H.label $ toHtml n
                                             br
                                             input ! type_ "number"
                                                   ! class_ "num-doub form-control doubleopt"
                                                   ! onchange (toValue ("handlechange(this, " ++ show l ++ "," ++ show h ++ ",true);"))
                                                   ! value (toValue $ (l + h) / 2)
                                         IntOpt n l h -> do
                                             H.label $ toHtml n
                                             br
                                             input ! type_ "number"
                                                   ! class_ "num-doub form-control intopt"
                                                   ! onchange (toValue ("handlechange(this, " ++ show l ++ "," ++ show h ++ ",false);"))
                                                   ! value (toValue $ (l + h) `Prelude.div` 2)
                                         ColorOpt n -> do
                                             H.label $ toHtml n
                                             br
                                             button ! class_ "btn btn-default colors coloropt" $ " "
                                         ColorList n -> do
                                             H.label $ toHtml n
                                         BoolOpt n -> do
                                             H.label $ toHtml n
                                         AnimOpt n -> do
                                             H.label $ toHtml n
                            )
