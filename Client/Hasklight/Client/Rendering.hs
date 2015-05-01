{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Hasklight.Client.Rendering where

import Text.Blaze.Html.Renderer.Text ( renderHtml )
import Text.Hamlet                   ( shamlet )
import Data.Text.Lazy                ( Text )

import Hasklight.AnimMetadata


renderAnimList :: [AnimMetadata] -> Text
renderAnimList lst = renderHtml [shamlet|
        $forall (i,AnimMetadata n _ _) <- lst'
            <tr>
                <td>
                    <a id="avail-#{i}" class="anim-selected" href="#" data-dismiss="modal" aria-label="Close">#{n}
        |]
    where lst' = (zip [0..] lst) :: [(Int,AnimMetadata)]

renderAnim :: AnimMetadata -> Text
renderAnim (AnimMetadata name opts _) = renderHtml [shamlet|$newline always
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
                    $forall (AnimParam n field) <- opts
                        <div class="col-md-4">
                            <div class="form-group form-inline">
                                <label class="field-name">#{n}
                                $case field
                                    $of AnimDouble d
                                        <input type="number" class="form-control opt" opttype="double" value=#{d}>
                                    $of AnimInt i
                                        <input type="number" class="form-control opt" opttype="int" value=#{i}>
                                    $of AnimLED r g b
                                        <input type="text" class="form-control opt colopt" opttype="color" value="rgb(#{div r 16}, #{div g 16}, #{div b 16})">
    |]
