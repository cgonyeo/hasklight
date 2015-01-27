module Animations.CylonEye where

import Animations.LED
import qualified Data.Vector as V

cylonEye :: Double -- speed
         -> Double -- size
         -> Color
         -> DisplaySize
         -> TimeDiff
         -> Display
cylonEye speed size c s t  = V.generate s (\x -> modC c $ animCos $ ((fromIntegral x) - (center $ t * speed) * (fromIntegral s)) / size)
    where center x = cosify $ abs ((decmComp x) * 2 - 1)
          cosify x = (cos ( (x + 1) * pi)) / 2 + 0.5
