{-# LANGUAGE DeriveDataTypeable #-}
module Site.JSON where

import System.Clock
import System.Random
import Animations.LED
import Text.JSON.Generic
import Animations.CylonEye
import Animations.Mirrors
import Animations.Ripple
import Animations.SetAll
import Animations.Spectrum
import Animations.Strobe
import Animations.Wave
import Animations.Volume

import qualified Data.Vector as V

data AnimMetadata = AnimMetadata { name         :: String
                                 , params       :: [AnimParam]
                                 , blendingmode :: String
                                 } deriving(Show, Data, Typeable)

data AnimParam = AnimDouble Double
               | AnimInt Int
               | AnimLED { red :: Int 
                         , green :: Int
                         , blue :: Int
                         }
               deriving(Show, Data, Typeable)

parseJSON :: String -> [AnimMetadata]
parseJSON = decodeJSON

writeJSON :: [AnimMetadata] -> String
writeJSON = encodeJSON

convMode :: String -> BlendingMode
convMode "Add"      = add
convMode "Subtract" = sub
convMode "Mask"     = mask
convMode _          = add

convColor :: LED -> LED
convColor (LED r g b) = (LED (f r) (f g) (f b))
    where f :: Int -> Int
          f x = floor $ (fromIntegral x :: Double) / 255 * 4095

convert :: AnimMetadata -> IO (Animation,BlendingMode)
convert AnimMetadata { name = "Cylon Eye"
                     , params = [ AnimDouble speed
                                , AnimDouble size
                                , AnimLED r g b
                                ]
                     , blendingmode = mode
                     }
          = return ( TimeOnly $ cylonEye speed size (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Set All"
                     , params = [ AnimLED r g b ]
                     , blendingmode = mode
                     }
          = return ( TimeOnly $ setAll (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Spectrum"
                     , params = [ AnimLED r g b ]
                     , blendingmode = mode
                     }
          = return ( FFT $ spectrum (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Wave"
                     , params = [ AnimDouble speed
                                , AnimDouble size
                                , AnimDouble freq
                                , AnimLED r g b
                                ]
                     , blendingmode = mode
                     }
          = return ( TimeOnly $ wave speed size freq (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Volume"
                     , params = [ AnimLED r g b ]
                     , blendingmode = mode
                     }
          = return ( FFT $ volume (LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Ripple"
                     , params = [ AnimDouble size
                                , AnimDouble freq
                                , AnimLED r g b
                                ]
                     , blendingmode = mode
                     }
          = do (TimeSpec s ns) <- getTime Monotonic
               let t = (fromIntegral s) + ((fromIntegral ns) / 10^(9 :: Int))
               rnd <- getStdGen
               return ( TimeOnly $ ripple size freq (V.fromList [(0,0)]) (convColor $ LED r g b) rnd
                   , convMode mode
                   )
convert _ = return (TimeOnly $ setAll (LED 0 0 0), add)
