module Animations.SetAll where

import Animations.LED

setAll :: Color
       -> DisplaySize
       -> TimeDiff
       -> Display
setAll c s _ = map (\_ -> c) [1..s]
