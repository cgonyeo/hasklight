module Hasklight.Animations.SetAll where

import qualified Data.Vector as V
import Hasklight.LED

setAll :: Color
       -> DisplaySize
       -> AnimInfo
       -> (Display,Animation)
setAll c s _ = (V.replicate s c,Animation $ setAll c)
