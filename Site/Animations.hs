module Site.Animations where

import Animations.LED
import Animations.CylonEye
import Animations.SetAll
import Animations.Spectrum
import Animations.Volume
import Animations.Strobe
import Animations.Wave

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

metaToAnims :: (AnimMetadata,BlendingMode) -> (Animation,BlendingMode)
metaToAnims (am,bl) = case am of
                          CylonEye a b c -> (TimeOnly $ cylonEye a b c,bl)
                          SetAll a       -> (TimeOnly $ setAll a,bl)
                          Spectrum a     -> (FFT $ spectrum a,bl)
                          Volume a       -> (Audio $ volume a,bl)
                          Wave a b c d   -> (TimeOnly $ wave a b c d,bl)

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
