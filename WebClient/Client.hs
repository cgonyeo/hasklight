{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import GHCJS.DOM                     ( runWebGUI
                                     , webViewGetDomDocument
                                     )
import GHCJS.DOM.Document            ( documentCreateElement
                                     , documentGetBody
                                     , documentGetElementById
                                     )
import GHCJS.DOM.Element             ( elementOnclick )
import GHCJS.DOM.HTMLElement         ( htmlElementSetInnerHTML
                                     , htmlElementSetInnerText
                                     )
import GHCJS.DOM.Node                ( nodeAppendChild )
import GHCJS.DOM.Types               ( castToHTMLAnchorElement
                                     , castToHTMLDivElement
                                     , castToHTMLHRElement
                                     , castToHTMLTableElement
                                     , Document
                                     , Element
                                     , GType
                                     , HTMLCanvasElement
                                     , HTMLDivElement
                                     )
import Control.Applicative           ( (<$>) )
import Control.Monad.IO.Class        ( liftIO )
import Data.Text.Lazy                ( Text
                                     , toStrict
                                     , unpack
                                     )
import Text.Blaze.Html.Renderer.Text ( renderHtml )
import Text.Hamlet                   ( shamlet )

import Site.Animations


main = do
    runWebGUI $ (\ webView -> do
        Just doc <- webViewGetDomDocument webView
        Just body <- documentGetBody doc

        Just animtbl <- fmap castToHTMLTableElement
                             <$> documentGetElementById doc "availanimlist"
        htmlElementSetInnerHTML animtbl (unpack $ renderAnimList availAnims)

        mapM_ (addAnimAction doc) [0..(length availAnims - 1)]
      )

addAnimAction :: Document -> Int -> IO ()
addAnimAction doc i = do
        Just btnsep  <- fmap castToHTMLHRElement
                             <$> documentGetElementById doc "btnsep"
        Just anim <- fmap castToHTMLAnchorElement
                            <$> documentGetElementById doc ("avail-" ++ show i)
        elementOnclick anim . liftIO $ do
            let anim = renderAnim (availAnims !! i)
            Just newdiv <- fmap castToHTMLDivElement
                                    <$> documentCreateElement doc "div"
            htmlElementSetInnerHTML newdiv (unpack anim)
            nodeAppendChild btnsep (Just newdiv)
            return ()
        return ()

renderAnimList :: [AvailAnim] -> Text
renderAnimList lst = renderHtml [shamlet|
        $forall (i,AvailAnim n _) <- lst'
            <tr>
                <td>
                    <a id="avail-#{i}" class="anim-selected" href="#" data-dismiss="modal" aria-label="Close">#{n}
        |]
    where lst' = (zip [0..] lst) :: [(Int,AvailAnim)]

renderAnim :: AvailAnim -> Text
renderAnim (AvailAnim name opts) = renderHtml [shamlet|$newline always
        <div class="panel panel-default">
            <div class="panel-body">
                <div class="row">
                    <div class="col-xs-8">
                        <h3>#{name}
                    <div class="col-xs-2">
                        <select class="form-control anim-bl">
                            $forall x <- blendingOpts
                                <option>#{x}
                    <div class="col-xs-2">
                        <button class="btn btn-default anim-up">
                            <span class="glyphicon glyphicon-menu-up">
                        <button class="btn btn-default anim-down">
                            <span class="glyphicon glyphicon-menu-down">
                        <button class="btn btn-default del-anim">
                            <span class="glyphicon glyphicon-remove">
                <div class="row">
                    $forall opt <- opts
                        <div class="col-md-4">
                            <div class="form-group form-inline">
                                $case opt
                                    $of DoubleOpt n l h
                                        <label>#{n}
                                        <input type="number" class="num-doub form-control doubleopt" value=#{(l + h) / 2}>
                                    $of IntOpt n l h
                                        <label>#{n}
                                        <input type="number" class="num-doub form-control doubleopt" value=#{div (l + h) 2}>
                                    $of ColorOpt n
                                        <label>#{n}
                                        <button class="btn btn-default colors coloropt" style="background:#ffffff">
                                    $of ColorList n
                                        <label>#{n}
                                    $of BoolOpt n
                                        <label>#{n}
                                    $of AnimOpt n
                                        <label>#{n}
    |]
