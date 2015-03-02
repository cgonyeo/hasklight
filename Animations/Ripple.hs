module Animations.Ripple where

import Animations.LED
import qualified Data.Vector as V

ripple :: Double -- wave size
       -> Double -- ripple frequency
       -> V.Vector (TimeDiff,Double) -- ripple starting times and locations
       -> Color
       -> DisplaySize
       -> TimeDiff
       -> (Display,Animation)
ripple size freq rpls (LED r g b) s t' = (cols,TimeOnly $ ripple size freq rpls (LED r g b))
        where wavefunc :: Double -> Double -> Int -> Double
              wavefunc t offset x = let pos = (fromIntegral x) - offset
                                    in if abs pos < t + pi
                                           then (-1) * size * sin (sqrt (pos^2) - t) / (sqrt (pos^2 + t^2))
                                           else 0
              strengths = V.map (\(t,x) -> V.generate s (wavefunc (10 * (t' - t)) x)) rpls
              strengths' = let combine = V.zipWith (\x y -> (x + y))
                           in V.foldr combine (V.replicate s 0.0) strengths
              f i d = i + (floor $ (fromIntegral i) * d)
              cols = V.map (\x -> LED (f r x) (f g x) (f b x)) strengths'
