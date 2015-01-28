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

type BlendingMode = Display -> Display -> Display

data Animation = TimeOnly (DisplaySize -> TimeDiff -> (Display,Animation))
               | Audio    (DisplaySize -> [Float]  -> (Display,Animation))
               | FFT      (DisplaySize -> [Float]  -> (Display,Animation))

ledBounds :: Int -> Int
ledBounds = (max 0) . (min 4095)

add :: Display -> Display -> Display
add = V.zipWith (\(LED r b g) (LED r' b' g') -> LED (f r r') (f b b') (f g g'))
    where f a b = ledBounds $ a + b

sub :: Display -> Display -> Display
sub = V.zipWith (\(LED r b g) (LED r' b' g') -> LED (f r r') (f b b') (f g g'))
    where f a b = ledBounds $ b - a

mult :: Display -> Display -> Display
mult = V.zipWith (\(LED r b g) (LED r' b' g') -> LED (f r r') (f b b') (f g g'))
    where f a b = ledBounds
                $ floor
                $ (fromIntegral b :: Double) * (fromIntegral a) / 4096

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
