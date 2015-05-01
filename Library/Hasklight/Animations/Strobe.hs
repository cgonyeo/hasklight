module Hasklight.Animations.Strobe where

import qualified Data.Vector as V
import Hasklight.LED

strobe :: Double --Speed
       -> Color
       -> DisplaySize
       -> AnimInfo
       -> (Display,Animation)
strobe speed c s i = if decmComp ((time i) * speed) > 0.5
                         then (V.replicate s c,Animation $ strobe speed c)
                         else (V.replicate s (LED 0 0 0),Animation $ strobe speed c)
