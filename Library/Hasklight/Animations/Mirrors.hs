module Hasklight.Animations.Mirrors where

import qualified Data.Vector as V
import Hasklight.LED

mirror :: (DisplaySize -> AnimInfo -> (Display,Animation))
       -> DisplaySize
       -> AnimInfo
       -> (Display,Animation)
mirror f s i = ((V.reverse half) V.++ half,Animation $ mirror anim)
        where (half,Animation anim) = f (s `div` 2) i
