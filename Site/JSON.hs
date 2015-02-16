{-# LANGUAGE DeriveDataTypeable #-}
module Site.JSON where

import Text.Parsec as P
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Char
import Control.Applicative
import Numeric
import Site.Animations
import Animations.LED
import Text.JSON.Generic
import Animations.SetAll
import Animations.Strobe
import Animations.CylonEye
import Animations.Wave
import Animations.Volume
import Animations.Spectrum
import Animations.Mirrors

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

metaToAnims :: [AnimMetadata] -> [(Animation,BlendingMode)]
metaToAnims = map convert
        where convMode "Add"      = add
              convMode "Subtract" = sub
              convMode "Multiply" = mult
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
              convert _ = (TimeOnly $ setAll (LED 0 0 0), add)
