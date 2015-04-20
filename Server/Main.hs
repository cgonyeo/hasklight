module Main where

import Control.Concurrent
import System.Clock
import Snap.Http.Server
import Snap.Core
import Data.ConfigFile
import System.Environment
import Data.Either.Utils

import qualified Data.Vector     as V
import qualified Data.ByteString as BS

import Hasklight.Audio
import Hasklight.LED
import Hasklight.TLC5947
import Hasklight.Site

main :: IO ()
main = do args <- getArgs
          case args of
              [file] -> do
                  --load the config
                  (snapconf,host,presetsdir) <- getConfig file
                  --initialize hardware
                  tlcInit
                  --set up for audio
                  rtCalls <- startAudio
                  --make some mvars
                  animmeta <- newMVar []
                  mvfft <- newMVar []
                  --run the FFT every time there's new audio
                  modifyMVar (\x -> return (runFFT mvfft : x)) rtCalls
                  --Start up the http server on a new thread
                  _ <- forkIO $ httpServe snapconf $ site al animmeta host presetsdir
                  --Drop into loop for hardware control
                  al <- startAnimList
                  runOdd al mvfft rtCalls 0 0
              _ -> putStrLn "Usage: hasklight <config file>"

getConfig :: FilePath -> IO (Config Snap a,String,FilePath)
getConfig file = do cfg <- forceEither `fmap` readfile emptyCP file
                    let host       = forceEither $ get cfg "DEFAULT" "host"
                        port       = forceEither $ get cfg "DEFAULT" "port"
                        accesslog  = forceEither $ get cfg "DEFAULT" "http_access_log" :: String
                        errorlog   = forceEither $ get cfg "DEFAULT" "http_error_log" :: String
                        presetsdir = forceEither $ get cfg "DEFAULT" "presets_dir"
                        snapconf   = setHostname (BS.pack host)
                                   $ setPort port
                                   $ setAccessLog (ConfigFileLog accesslog)
                                   $ setErrorLog (ConfigFileLog errorlog)
                                   $ defaultConfig
                    return (snapconf,show host,presetsdir)

startAnimList :: IO (MVar (V.Vector (Animation,BlendingMode)))
startAnimList = newMVar $ V.fromList []

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

runOdd :: MVar (V.Vector (Animation,BlendingMode)) -> MVar [Float] -> TimeDiff -> Int -> IO ()
runOdd animListM mvfft t' c = do
        (TimeSpec s ns) <- getTime Monotonic
        sound' <- getSoundBuffer
        fftvals <- readMVar mvfft
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
        --print $ V.toList $ V.map (\(LED r g b) -> r) disp
        updateDisp disp
        putMVar animListM animList'
        if t - t' >= 1
            then do putStrLn $ "Current framerate: " ++ show c
                    runOdd animListM t 0
            else runOdd animListM t' (c+1)
