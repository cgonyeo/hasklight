{-# LANGUAGE DeriveDataTypeable #-}
module Hasklight.AnimParams where

import Text.JSON.Generic
import Hasklight.LED

data AvailAnim = AvailAnim { animName :: String
                           , animOpts :: [AnimOpt]
                           } deriving(Show, Data, Typeable)

data AnimOpt = DoubleOpt { dname :: String
                         , dmin  :: Double
                         , dmax  :: Double
                         }
             | IntOpt { iname :: String
                      , imin  :: Int
                      , imax  :: Int
                      }
             | ColorOpt { cname :: String }
             | ColorList { clname :: String }
             | BoolOpt { bname :: String }
             | AnimOpt { aname :: String }
             deriving(Show, Data, Typeable)

blendingOpts :: [String]
blendingOpts = [ "Add"
               , "Subtract"
               , "Mask"
               ]

availAnims :: [AvailAnim]
availAnims = [ AvailAnim "Cylon Eye" [ DoubleOpt "Speed" 0.01 10
                                     , DoubleOpt "Size "0.5 64
                                     , ColorOpt "Color"
                                     ]
             --, AvailAnim "Mirror"    [ AnimOpt "Animation" ]
             , AvailAnim "Ripple"    [ DoubleOpt "Wave Size" 0 100000
                                     , DoubleOpt "Frequency" 0 100000
                                     , ColorOpt "Color"
                                     ]
             , AvailAnim "Set All"   [ ColorOpt "Color" ]
             , AvailAnim "Spectrum"  [ ColorOpt "Color" ]
             , AvailAnim "Volume"    [ ColorOpt "Color" ]
             , AvailAnim "Wave"      [ DoubleOpt "Speed" 0.01 10
                                     , DoubleOpt "Size" 0.5 64
                                     , DoubleOpt "Frequency" 0.1 8
                                     , ColorOpt "Color"
                                     ]
             ]
