import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import System.Clock

import Audio.Audio
import Animations.LED
import Animations.SetAll
import Animations.CylonEye
import Animations.Wave
import Animations.Volume
import TLC5947.TLC5947

animList :: [Animation]
animList = [ (setAll (LED 0 0 0) 63)
           --, (cylonEye 1 5 (LED 300 0 0) 63)
           --, (wave 0.2 2 1 (LED 70 0 0) 29)
           ]

audioAnimList :: [AudioAnimation]
audioAnimList = [volume (LED 0 2000 2000) 64]


updateDisp :: Display -> IO ()
updateDisp disp = do tlcClearLeds
                     forM_ (zip disp [0..(length disp)]) 
                             (\((LED r g b),i) -> do tlcSetLed (i * 3) r
                                                     tlcSetLed (i * 3 + 1) g
                                                     tlcSetLed (i * 3 + 2) b
                             )
                     tlcUpdateLeds

main :: IO ()
main = do putStrLn "Initializing hardware..."
          tlcInit
          putStrLn "Initializing audio..."
          forkIO audioInitialization
          putStrLn "Listening started. Commencing with pretty lights..."
          runOdd

runOdd :: IO ()
runOdd = do (TimeSpec s ns) <- getTime Monotonic
            sound <- getSoundBuffer
            let t = (fromIntegral s) + ((fromIntegral ns) / 10^9)
                (anim:anims) = map (\f -> f t) animList
                audAnims = map (\f -> f sound t) audioAnimList
                disp = foldr add (foldr add anim anims) audAnims
            updateDisp disp
            runOdd
