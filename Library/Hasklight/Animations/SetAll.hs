module Hasklight.Animations.SetAll where

import qualified Data.Vector as V
import Hasklight.LED

setAll :: Color
       -> DisplaySize
       -> TimeDiff
       -> (Display,Animation)
setAll c s _ = (V.replicate s c,TimeOnly $ setAll c)
