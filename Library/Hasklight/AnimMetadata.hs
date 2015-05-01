{-# LANGUAGE DeriveDataTypeable #-}
module Hasklight.AnimMetadata where

import Text.JSON.Generic
import Hasklight.LED


data AnimMetadata = AnimMetadata { name         :: String
                                 , params       :: [AnimParam]
                                 , blendingmode :: String
                                 } deriving(Show, Data, Typeable)

data AnimParam = AnimParam { param :: String
                           , field :: AnimField
                           } deriving(Show, Data, Typeable)
               
data AnimField = AnimDouble Double
               | AnimInt Int
               | AnimLED { red :: Int 
                         , green :: Int
                         , blue :: Int
                         }
               deriving(Show, Data, Typeable)

availAnims :: [AnimMetadata]
availAnims =
    [ AnimMetadata "Cylon Eye"
                   [ AnimParam "Speed" (AnimDouble 0.1)
                   , AnimParam "Size"  (AnimDouble 16)
                   , AnimParam "Color" (AnimLED 4095 0 0)
                   ]
                   "Add"
    , AnimMetadata "Ripple" 
                   [ AnimParam "Wave Size" (AnimDouble 16)
                   , AnimParam "Frequency" (AnimDouble 0.3)
                   , AnimParam "Color"     (AnimLED 4095 0 0)
                   ]
                   "Add"
    , AnimMetadata "Set All" 
                   [ AnimParam "Color" (AnimLED 4095 0 0) ]
                   "Add"
    , AnimMetadata "Spectrum" 
                   [ AnimParam "Color" (AnimLED 4095 0 0) ]
                   "Add"
    , AnimMetadata "Volume" 
                   [ AnimParam "Color" (AnimLED 4095 0 0) ]
                   "Add"
    , AnimMetadata "Wave" 
                   [ AnimParam "Speed"     (AnimDouble 0.3)
                   , AnimParam "Size"      (AnimDouble 16)
                   , AnimParam "Frequency" (AnimDouble 3)
                   , AnimParam "Color"     (AnimLED 4095 0 0)
                   ]
                   "Add"
    ]

blendingOpts :: [String]
blendingOpts = [ "Add"
               , "Subtract"
               , "Mask"
               ]
