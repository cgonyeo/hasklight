{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Hasklight.Client.Rendering where

import Text.Blaze.Html.Renderer.Text ( renderHtml )
import Text.Hamlet                   ( shamlet )
import Data.Text.Lazy                ( Text )

import Hasklight.AnimParams


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
        <div class="panel panel-default animdata">
            <div class="panel-body">
                <div class="row">
                    <div class="col-xs-8">
                        <h3 class="name">#{name}
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
                                        <input type="number" class="form-control opt" opttype="double" value=#{(l + h) / 2}>
                                    $of IntOpt n l h
                                        <label>#{n}
                                        <input type="number" class="form-control opt" opttype="int" value=#{div (l + h) 2}>
                                    $of ColorOpt n
                                        <label>#{n}
                                        <input type="text" class="form-control opt colopt" opttype="color" value="rgb(255, 0, 0)">
                                    $of ColorList n
                                        <label>#{n}
                                    $of BoolOpt n
                                        <label>#{n}
                                    $of AnimOpt n
                                        <label>#{n}
    |]
