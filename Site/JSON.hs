{-# LANGUAGE DeriveDataTypeable #-}
module Site.JSON where

import Animations.LED
import Text.JSON.Generic
import Animations.CylonEye
import Animations.Mirrors
import Animations.SetAll
import Animations.Spectrum
import Animations.Strobe
import Animations.Wave
import Animations.Volume

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

metaToAnims :: [AnimMetadata] -> [(Animation,BlendingMode)]
metaToAnims = map convert

convMode :: String -> BlendingMode
convMode "Add"      = add
convMode "Subtract" = sub
convMode "Multiply" = mult
convMode _          = add

convert :: AnimMetadata -> (Animation,BlendingMode)
convert AnimMetadata { name = "Cylon Eye"
                     , params = [ AnimDouble speed
                                , AnimDouble size
                                , AnimLED r g b
                                ]
                     , blendingmode = mode
                     }
          = ( TimeOnly $ cylonEye speed size (LED r g b)
            , convMode mode
            )
convert AnimMetadata { name = "Set All"
                     , params = [ AnimLED r g b ]
                     , blendingmode = mode
                     }
          = ( TimeOnly $ setAll (LED r g b)
            , convMode mode
            )
convert AnimMetadata { name = "Spectrum"
                     , params = [ AnimLED r g b ]
                     , blendingmode = mode
                     }
          = ( FFT $ spectrum (LED r g b)
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
          = ( TimeOnly $ wave speed size freq (LED r g b)
            , convMode mode
            )
convert AnimMetadata { name = "Volume"
                     , params = [ AnimLED r g b ]
                     , blendingmode = mode
                     }
          = ( FFT $ volume (LED r g b)
            , convMode mode
            )
convert _ = (TimeOnly $ setAll (LED 0 0 0), add)
