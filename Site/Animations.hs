module Site.Animations where

import Animations.LED

data AvailAnim = AvailAnim { animName :: String
                           , animOpts :: [AnimOpt]
}

data AnimOpt = DoubleOpt Double Double
             | IntOpt Int Int
             | ColorOpt
             | ColorList
             | BoolOpt
             | AnimOpt
             | AnimList

data AnimMetadata = CylonEye Double Double Color
                  | Mirror AnimMetadata
                  | SetAll Color
                  | Spectrum Color
                  | Volume Color
                  | Wave Double Double Double Color

blendingOpts :: [String]
blendingOpts = [ "Add"
               , "Subtract"
               , "Multiply"
               ]

availAnims :: [AvailAnim]
availAnims = [ AvailAnim "Cylon Eye" [ DoubleOpt 0.01 10
                                     , DoubleOpt 0.5 64
                                     , ColorOpt
                                     ]
             , AvailAnim "Mirror"    [ AnimOpt ]
             , AvailAnim "Set All"   [ ColorOpt ]
             , AvailAnim "Spectrum"  [ ColorOpt ]
             , AvailAnim "Volume"    [ ColorOpt ]
             , AvailAnim "Wave"      [ DoubleOpt 0.01 10
                                     , DoubleOpt 0.5 64
                                     , DoubleOpt 0.1 8
                                     , ColorOpt
                                     ]
             ]
