module Animations.Mirrors where

import qualified Data.Vector as V
import Animations.LED

addMirror :: Animation -> Animation
addMirror a = case a of
                  TimeOnly f -> TimeOnly $ timeMirror f
                  Audio f -> Audio $ audioMirror f
                  FFT f -> FFT $ fftMirror f

timeMirror :: (DisplaySize -> TimeDiff -> (Display,Animation))
           -> DisplaySize
           -> TimeDiff
           -> (Display,Animation)
timeMirror f s t = ((V.reverse half) V.++ half,addMirror anim)
        where (half,anim) = f (s `div` 2) t

audioMirror :: (DisplaySize -> [Float] -> (Display,Animation))
            -> DisplaySize
            -> [Float]
            -> (Display,Animation)
audioMirror f s a = ((V.reverse half) V.++ half,addMirror anim)
        where (half,anim) = f (s `div` 2) a

fftMirror :: (DisplaySize -> [Float] -> (Display,Animation))
          -> DisplaySize
          -> [Float]
          -> (Display,Animation)
fftMirror f s a = ((V.reverse half) V.++ half,addMirror anim)
        where (half,anim) = f (s `div` 2) a
