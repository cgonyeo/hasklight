module Hasklight.Animations.Beat where

import qualified Data.Vector as V
import Hasklight.LED

beat :: Maybe TimeDiff          -- Start time (if any)
     -> TimeDiff                -- Current (ish) time
     -> [(TimeDiff,TimeDiff)]   -- Starts and ends for recent beats
     -> Color
     -> DisplaySize
     -> Bool
     -> (Display,Animation)
beat = undefined
