module Animations.SetAll where

import qualified Data.Vector as V
import Animations.LED

setAll :: Color
       -> DisplaySize
       -> TimeDiff
       -> (Display,Animation)
setAll c s _ = (V.replicate s c,TimeOnly $ setAll c)
