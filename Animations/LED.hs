module Animations.LED where

import qualified Data.Vector as V

data LED = LED { red   :: Int
               , blue  :: Int
               , green :: Int
               } deriving(Show,Eq)

type Display = V.Vector LED

type DisplaySize = Int

type Color = LED

type TimeDiff = Double

data Animation = TimeOnly (TimeDiff -> Display)
               | Audio    ([Float] -> Display)
               | FFT      ([Float] -> Display)

add :: Display -> Display -> Display
add = V.zipWith (\(LED r b g) (LED r' b' g') -> LED (f r r') (f b b') (f g g'))
    where f a b = if a + b < 4096
                      then a + b
                      else 4095

sub :: Display -> Display -> Display
sub = V.zipWith (\(LED r b g) (LED r' b' g') -> LED (f r r') (f b b') (f g g'))
    where f a b = if a - b >= 0
                      then a - b
                      else 0

--Scales an Integer value by a given amount, and keeps it within a range of
--valid LED values (a 12 bit positive integer).
scaleInt :: Int -> Double -> Int
scaleInt x y
    | x' < 0    = x
    | x' > 4095 = 4095
    | otherwise = x'
    where x' = (truncate $ (fromIntegral x) * y)

modC :: Color -> Double -> LED
modC (LED r g b) s = LED (scaleInt r s) (scaleInt g s) (scaleInt b s)

--Piecewise cosine function. If between -0.5 and 0.5, curves from 0 at
-- -0.5, to 1 at 0, and back to 0 at 0.5, and is 0 at all other places.
animCos :: (Floating a,Fractional a,Ord a) => a -> a
animCos x = if x >= (-0.5) && x <= 0.5
                then ((cos ((x + 0.5) * 2 * pi)) / 2 - 0.5) * (-1)
                else 0

--Takes a given time, and returns only the decimal component of the number.
--For example, 342.5453 becomes 0.5453.
decmComp :: TimeDiff -> Double
decmComp t = (t - (fromIntegral (floor t :: Int)))
