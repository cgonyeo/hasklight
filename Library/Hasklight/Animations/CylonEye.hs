module Hasklight.Animations.CylonEye where

import qualified Data.Vector as V
import Hasklight.LED

cylonEye :: Double -- speed
         -> Double -- size
         -> Color
         -> DisplaySize
         -> AnimInfo
         -> (Display,Animation)
cylonEye speed size c s i = 
    let center x = cosify $ abs ((decmComp x) * 2 - 1)
        cosify x = (cos ( (x + 1) * pi)) / 2 + 0.5
        disp = V.generate s (\x -> modC c $ animCos $ (
                                              (fromIntegral x)
                                            - (center $ (time i) * speed)
                                            * (fromIntegral s)
                                         ) / size)
    in (disp,Animation $ cylonEye speed size c)
