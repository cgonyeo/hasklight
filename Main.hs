import Control.Concurrent
import System.Clock
import qualified Data.Vector as V

import Audio.Audio
import Animations.LED
import Animations.SetAll
import Animations.CylonEye
--import Animations.Wave
--import Animations.Volume
--import Animations.Spectrum
import TLC5947.TLC5947

animList :: V.Vector Animation
animList = V.fromList [ TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      , TimeOnly (cylonEye 0.3 4 (LED 100 0 0) 64)
                      --, TimeOnly (wave 0.2 2 1 (LED 70 0 0) 64)
                      --, Audio    (volume (LED 0 0 0) 64)
                      ]

emptyDisplay :: Display
emptyDisplay = V.replicate 64 (LED 0 0 0)

updateDisp :: Display -> IO ()
updateDisp disp = do tlcClearLeds
                     V.forM_ (V.zip disp (V.generate (V.length disp) id)) 
                               (\((LED r g b),i) -> do tlcSetLed (i * 3) r
                                                       tlcSetLed (i * 3 + 1) g
                                                       tlcSetLed (i * 3 + 2) b
                               )
                     tlcUpdateLeds

main :: IO ()
main = do tlcInit
          audioInitialization
          runOdd 0 0

runOdd :: TimeDiff -> Int -> IO ()
runOdd t' c = do (TimeSpec s ns) <- getTime Monotonic
                 sound <- getSoundBuffer
                 fftvals <- runFFT
                 let t = (fromIntegral s) + ((fromIntegral ns) / 10^(9 :: Int))
                     disp = V.foldr (\x a -> case x of
                                                 TimeOnly f -> add (f t) a
                                                 Audio    f -> add (f sound) a
                                                 FFT      f -> add (f fftvals) a
                                    ) emptyDisplay animList
                 updateDisp disp
                 if t - t' >= 1
                     then do putStrLn $ "Current framerate: " ++ show c
                             runOdd t 0
                     else runOdd t' (c+1)
