module Site.Animations where

import Animations.LED

data AvailAnim = AvailAnim { animName :: String
                           , animOpts :: [AnimOpt]
                           }

data AnimOpt = DoubleOpt String Double Double
             | IntOpt String Int Int
             | ColorOpt String
             | ColorList String
             | BoolOpt String
             | AnimOpt String

data AnimMetadata = CylonEye Double Double Color
                  | Mirror AnimMetadata
                  | SetAll Color
                  | Spectrum Color
                  | Volume Color
                  | Wave Double Double Double Color
                  deriving(Show,Eq)

blendingOpts :: [String]
blendingOpts = [ "Add"
               , "Subtract"
               , "Multiply"
               ]

availAnims :: [AvailAnim]
availAnims = [ AvailAnim "Cylon Eye" [ DoubleOpt "Speed" 0.01 10
                                     , DoubleOpt "Size "0.5 64
                                     , ColorOpt "Color"
                                     ]
             , AvailAnim "Mirror"    [ AnimOpt "Animation" ]
             , AvailAnim "Set All"   [ ColorOpt "Color" ]
             , AvailAnim "Spectrum"  [ ColorOpt "Color" ]
             , AvailAnim "Volume"    [ ColorOpt "Color" ]
             , AvailAnim "Wave"      [ DoubleOpt "Speed" 0.01 10
                                     , DoubleOpt "Size" 0.5 64
                                     , DoubleOpt "Frequency" 0.1 8
                                     , ColorOpt "Color"
                                     ]
             ]
