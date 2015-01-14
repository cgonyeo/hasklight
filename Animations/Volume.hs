module Animations.Volume where

import Animations.LED
import GHC.Float

volume :: Color
       -> DisplaySize
       -> [Float]
       -> TimeDiff
       -> Display
volume (LED r g b) size sound _ = colors
        where strength = foldr (\x a -> (abs x) + a) 0 sound / (fromIntegral $ length sound)
              scale = 1.5 * (log $ strength * 100) / 6.643856
              size' = fromIntegral size
              s' = float2Double scale
              c = LED (modColor r s') (modColor g s') (modColor b s')
              colors = map (\x -> if x / size' < scale
                                      then c else (LED 0 0 0)) [1..size']
