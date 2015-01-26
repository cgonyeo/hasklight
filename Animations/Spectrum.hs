module Animations.Spectrum where

import Animations.LED
import GHC.Float

spectrum :: Color
         -> DisplaySize
         -> [Float]
         -> Display
spectrum c size bands = colors
        where s' = fromIntegral size :: Double
              (v:vs) = (map (\x -> floor $ (x / s') ** 2 * (1024)) [0..(s' - 1)]) ++ [1024]
              ranges = map (\(x,y) -> if x /= y then (x,y) else (x,y+1)) $ zip (v:vs) vs
              values = map (\(x,y) -> (take (y-x) . drop x) bands) ranges
              strengths = map (\x -> (foldr (+) 0.0 x) / (fromIntegral $ length x)) values
              colors = map (modC c . float2Double) strengths
