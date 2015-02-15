module Animations.Strobe where

import qualified Data.Vector as V
import Animations.LED

strobe :: Double --Speed
       -> Color
       -> DisplaySize
       -> TimeDiff
       -> (Display,Animation)
strobe speed c s t = if decmComp (t * speed) > 0.5
                         then (V.replicate s c,TimeOnly $ strobe speed c)
                         else (V.replicate s (LED 0 0 0),TimeOnly $ strobe speed c)
