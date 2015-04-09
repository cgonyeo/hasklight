module Hasklight.Animations.Volume where

import qualified Data.Vector as V
import GHC.Float
import Hasklight.LED

volume :: Color
       -> DisplaySize
       -> [Float]
       -> (Display,Animation)
volume c size sound = (colors,Audio $ volume c)
    where strength = foldr (\x a -> (abs x) + a) 0 sound
                   / (fromIntegral $ length sound)
          scale = float2Double $ 1.3 * (log $ strength * 100) / 6.643856
          colors = V.generate size
                        (\x -> if (fromIntegral x) / (fromIntegral size) < scale
                                   then modC c scale
                                   else (LED 0 0 0))
