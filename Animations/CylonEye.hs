module Animations.CylonEye where

import Animations.LED

cylonEye :: Double -- speed
         -> Double -- size
         -> Color
         -> DisplaySize
         -> TimeDiff
         -> Display
cylonEye speed size (LED r g b) s t = colors
    where cosify x = (cos ( (x + 1) * pi)) / 2 + 0.5
          center = abs $ (cosify $ ((decmComp $ speed * t) - 0.5) * 2) * (fromIntegral s)
          distances = map (\x -> abs ((x - (center)) / size)) [0,1..(fromIntegral s)]
          mcCos x y = modColor x (animCos y)
          colors = map (\y -> LED (mcCos r y) (mcCos g y) (mcCos b y)) distances
