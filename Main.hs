import Control.Concurrent
import System.Clock
import qualified Data.Vector as V

import Audio.Audio
import Animations.LED
import Animations.SetAll
import Animations.CylonEye
import Animations.Wave
import Animations.Volume
import Animations.Spectrum
import Animations.Mirrors
import TLC5947.TLC5947

startAnimList :: V.Vector (Animation,BlendingMode)
startAnimList = V.fromList [ (TimeOnly (setAll (LED 20 0 0)),add)
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 1 4 1 (LED 100 0 0))
                      --, TimeOnly (wave 0.2 2 1 (LED 70 0 0))
                      --, Audio (volume (LED 20 0 0))
                      --, FFT (spectrum (LED 20 0 0))
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
          runOdd startAnimList 0 0

runOdd :: V.Vector (Animation,BlendingMode) -> TimeDiff -> Int -> IO ()
runOdd animList t' c = do
        (TimeSpec s ns) <- getTime Monotonic
        sound <- getSoundBuffer
        fftvals <- runFFT
        let t = (fromIntegral s) + ((fromIntegral ns) / 10^(9 :: Int))
            (layers,animList') = V.unzip $ 
                V.map (\x -> case x of
                                 (TimeOnly f,bl) -> let (dis,anim) = (f 64 t)
                                                    in (bl dis,(anim,bl))
                                 (Audio f,bl) -> let (dis,anim) = (f 64 sound)
                                                    in (bl dis,(anim,bl))
                                 (FFT f,bl) -> let (dis,anim) = (f 64 fftvals)
                                                    in (bl dis,(anim,bl))
                      ) animList
            disp = V.foldr' (\x a -> x a) emptyDisplay layers
        updateDisp disp
        print $ V.toList $ V.map (\(LED r _ _) -> r) disp
        if t - t' >= 1
            then do putStrLn $ "Current framerate: " ++ show c
                    runOdd animList' t 0
            else runOdd animList' t' (c+1)
