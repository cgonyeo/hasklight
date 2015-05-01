{-# LANGUAGE DeriveDataTypeable #-}
module Hasklight.Convert where

import System.Clock
import System.Random
import Text.JSON.Generic
import Hasklight.LED
import Hasklight.Animations
import Hasklight.AnimMetadata

import qualified Data.Vector as V

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
convColor (LED r g b) = (LED (16 * r) (16 * g) (16 * b))

convert :: AnimMetadata -> IO (Animation,BlendingMode)
convert AnimMetadata { name = "Cylon Eye"
                     , params = [ AnimParam "Speed" (AnimDouble speed)
                                , AnimParam "Size"  (AnimDouble size)
                                , AnimParam "Color" (AnimLED r g b)
                                ]
                     , blendingmode = mode
                     }
          = return ( Animation $ cylonEye speed size (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Set All"
                     , params = [ AnimParam "Color" (AnimLED r g b) ]
                     , blendingmode = mode
                     }
          = return ( Animation $ setAll (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Spectrum"
                     , params = [ AnimParam "Color" (AnimLED r g b) ]
                     , blendingmode = mode
                     }
          = return ( Animation $ spectrum (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Wave"
                     , params = [ AnimParam "Speed"     (AnimDouble speed)
                                , AnimParam "Size"      (AnimDouble size)
                                , AnimParam "Frequency" (AnimDouble freq)
                                , AnimParam "Color"     (AnimLED r g b)
                                ]
                     , blendingmode = mode
                     }
          = return ( Animation $ wave speed size freq (convColor $ LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Volume"
                     , params = [ AnimParam "Color" (AnimLED r g b) ]
                     , blendingmode = mode
                     }
          = return ( Animation $ volume (LED r g b)
                   , convMode mode
                   )
convert AnimMetadata { name = "Ripple"
                     , params = [ AnimParam "Size"      (AnimDouble size)
                                , AnimParam "Frequency" (AnimDouble freq)
                                , AnimParam "Color"     (AnimLED r g b)
                                ]
                     , blendingmode = mode
                     }
          = do (TimeSpec s ns) <- getTime Monotonic
               let t = (fromIntegral s) + ((fromIntegral ns) / 10^(9 :: Int))
               rnd <- getStdGen
               return ( Animation $ ripple size freq (V.fromList [(0,0)]) (convColor $ LED r g b) rnd
                   , convMode mode
                   )
convert _ = return (Animation $ setAll (LED 0 0 0), add)
