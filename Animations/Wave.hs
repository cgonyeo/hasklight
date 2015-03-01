module Animations.Wave where

import qualified Data.Vector as V
import Animations.LED

wave :: Double -- speed
     -> Double -- size
     -> Double -- frequency
     -> Color
     -> DisplaySize
     -> TimeDiff
     -> (Display,Animation)
wave speed size freq c s t = (disp, TimeOnly $ wave speed size freq c)
    where centers = V.generate (ceiling freq)
                         (\x ->
                           (decmComp
                               (if freq >= 1
                                    then t * speed
                                    else t * speed * freq
                               )
                           ) / freq + ((fromIntegral x) / freq)
                         )
          centers' = let offset = size / (fromIntegral s)
                     in V.map (\x -> x * (2 * offset + 1) - offset) centers
          disp = V.generate s 
                         (\x -> modC c $ animCos $
                           ((fromIntegral x)
                            - (V.foldr' (\y a
                                          -> let loc = (fromIntegral x)
                                                     / (fromIntegral s)
                                             in if abs (a - loc) < abs (y - loc)
                                                    then a
                                                    else y
                                         ) 2 centers')
                            * (fromIntegral s)
                           ) / size)
