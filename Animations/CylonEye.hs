module Animations.CylonEye where

import Animations.LED
import qualified Data.Vector as V

cylonEye :: Double -- speed
         -> Double -- size
         -> Color
         -> DisplaySize
         -> TimeDiff
         -> Display
cylonEye speed size (LED r g b) s t = colors
    where cosify x = (cos ( (x + 1) * pi)) / 2 + 0.5
          center = abs $ (cosify $ ((decmComp $ speed * t) - 0.5) * 2) * (fromIntegral s)
          distances = V.generate s (\x -> abs (((fromIntegral x) - (center)) / size))
          mcCos x y = scaleInt x (animCos y)
          colors = V.map (\y -> LED (mcCos r y) (mcCos g y) (mcCos b y)) distances
