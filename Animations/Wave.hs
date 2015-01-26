module Animations.Wave where

import Animations.LED

wave :: Double -- speed
     -> Double -- size
     -> Double -- frequency
     -> Color
     -> DisplaySize
     -> TimeDiff
     -> Display
wave speed size freq c s t = colors
        where center = decmComp $ speed * t
              strengths = map (wavefunc s center) [0..(fromIntegral (s - 1))]
              colors = map (modC c) strengths

wavefunc :: DisplaySize -> Double -> Int -> Double
wavefunc l c x = animCos distance
        where fx = (fromIntegral x) / (fromIntegral l)
              x' = if x < l `div` 2
                       then (0.5 - fx) * 2 -- from 0->1 to 0.5->0
                       else (fx - 0.5) * 2 -- from 0->1 to 0.5->1
              distance = (x' - (c * 2 - 0.5))
