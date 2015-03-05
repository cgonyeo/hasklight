module Animations.Ripple where

import Animations.LED
import qualified Data.Vector as V
import System.Random

ripple :: Double -- wave size
       -> Double -- ripple frequency
       -> V.Vector (TimeDiff,Double) -- ripple starting times and locations
       -> Color
       -> StdGen
       -> DisplaySize
       -> TimeDiff
       -> (Display,Animation)
ripple size freq rpls (LED r g b) rnd s t' = if fst (V.head rpls) < t'
                                                 then let (n,rnd') = randomR (0,s*2) rnd
                                                          time = t' + (1 / (freq * 2)) + (1 / freq) * ((fromIntegral n) / (fromIntegral $ s*2))
                                                          (n',rnd'') = randomR (0,s*2) rnd'
                                                          pos = (fromIntegral n') / 2
                                                      in (cols,TimeOnly $ ripple size freq (V.cons (time,pos) rpls) (LED r g b) rnd'')
                                                 else (cols,TimeOnly $ ripple size freq (V.filter (\(t,_) -> t' - t < 10) rpls) (LED r g b) rnd)
        where wavefunc :: Double -> Double -> Int -> Double
              wavefunc t offset x = let pos = (fromIntegral x) - offset
                                    in if abs pos < t + pi
                                           then (-1) * size * sin (sqrt (pos^2) - t) / (sqrt (pos^2 + t^2))
                                           else 0
              strengths = V.map (\(t,x) -> V.generate s (wavefunc (15 * (t' - t)) x)) rpls
              f i i' d = i + (floor $ (fromIntegral i') * d)
              cols = let combine = V.zipWith (\x (LED r' g' b') -> (LED (f r' r x) (f g' g x) (f b' b x)))
                     in V.foldr combine (V.replicate s (LED r g b)) strengths
