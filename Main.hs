import Control.Concurrent
import System.Clock
import qualified Data.Vector as V
import Snap.Http.Server

import Audio.Audio
import Animations.LED
import TLC5947.TLC5947
import Site.Site

startAnimList :: IO (MVar (V.Vector (Animation,BlendingMode)))
startAnimList = newMVar $
           V.fromList [ --(TimeOnly (cylonEye 0.4 10 (LED 500 0 0)),add)
                      --, (Audio $ audioMirror $ volume (LED 1000 0 0), add)
                      --, (FFT $ spectrum (LED 0 1000 2000),add)
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
          al <- startAnimList
          _ <- forkIO $ quickHttpServe $ site al
          runOdd al 0 0

runOdd :: MVar (V.Vector (Animation,BlendingMode)) -> TimeDiff -> Int -> IO ()
runOdd animListM t' c = do
        (TimeSpec s ns) <- getTime Monotonic
        sound' <- getSoundBuffer
        fftvals <- runFFT
        animList <- takeMVar animListM
        let t = (fromIntegral s) + ((fromIntegral ns) / 10^(9 :: Int))
            sound = drop (length sound' `div` 2) sound'
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
        --print $ V.toList $ dfgsdfg fftvals
        --putStrLn "------"
        updateDisp disp
        putMVar animListM animList'
        if t - t' >= 1
            then do putStrLn $ "Current framerate: " ++ show c
                    runOdd animListM t 0
            else runOdd animListM t' (c+1)
