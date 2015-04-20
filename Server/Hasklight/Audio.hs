{-# LANGUAGE ForeignFunctionInterface #-}
module Hasklight.Audio where

import control.Concurrent.MVar
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "audioInitialization" audioInitialization :: IO (Ptr CInt)
foreign import ccall "getSoundBuffer" getSoundBuffer_c :: IO (Ptr CFloat)
foreign import ccall "runFFT" runFFT_c :: IO (Ptr CFloat)
foreign import ccall "processAudio" processAudio_c :: (Ptr CInt) -> IO ()

getSoundBuffer :: IO [Float]
getSoundBuffer = do sptr <- getSoundBuffer_c
                    mapM (\x -> peek $ plusPtr sptr (x * 4)) [0..1023]

runFFT :: MVar [Float] -> IO ()
runFFT mv = do
        sptr <- runFFT_c
        vals <- mapM (\x -> peek $ plusPtr sptr (x * 4)) [0..1023]
        modifyMVar (\_ -> return vals) mv

startAudio :: IO (MVar [IO ()])
startAudio = do
        mv <- newMVar []
        captureHandle <- audioInitialization
        forkIO $ audioLoop captureHandle mv
        return mv

audioLoop :: Ptr CInt -> MVar [IO ()] -> IO ()
audioLoop captureHandle mv = do
        processAudio_c captureHandle
        readMVar mv >>= mapM_ (\x -> x)
        processAudio captureHandle mv
