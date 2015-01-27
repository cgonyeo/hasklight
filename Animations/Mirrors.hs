module Animations.Mirrors where

import qualified Data.Vector as V
import Animations.LED

timeMirror :: (DisplaySize -> TimeDiff -> Display)
           -> DisplaySize
           -> TimeDiff
           -> Display
timeMirror f s t = (V.reverse half) V.++ half
        where half = f (s `div` 2) t

audioMirror :: (DisplaySize -> [Float] -> Display)
            -> DisplaySize
            -> [Float]
            -> Display
audioMirror f s a = (V.reverse half) V.++ half
        where half = f (s `div` 2) a

fftMirror :: (DisplaySize -> [Float] -> Display)
          -> DisplaySize
          -> [Float]
          -> Display
fftMirror = audioMirror
