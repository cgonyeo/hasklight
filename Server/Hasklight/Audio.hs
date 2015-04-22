{-# LANGUAGE ForeignFunctionInterface #-}
module Hasklight.Audio where

import Control.Concurrent
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "audioInitialization" audioInitialization :: IO (Ptr CInt)
foreign import ccall "getSoundBuffer" getSoundBuffer_c :: IO (Ptr (Ptr CFloat))
foreign import ccall "runFFT" runFFT_c :: IO (Ptr (Ptr CFloat))
foreign import ccall "processAudio" processAudio_c :: (Ptr CInt) -> IO ()

getSoundBuffer :: IO ([Float],[Float])
getSoundBuffer = do
        sptr <- getSoundBuffer_c
        lptr <- peek sptr
        rptr <- peek $ plusPtr sptr 4
        lvals <- mapM (\x -> peek $ plusPtr lptr (x * 4)) [0..1023]
        rvals <- mapM (\x -> peek $ plusPtr rptr (x * 4)) [0..1023]
        return (lvals,rvals)

runFFT :: MVar ([Float],[Float]) -> IO ()
runFFT mv = do
        sptr <- runFFT_c
        lptr <- peek sptr
        rptr <- peek $ plusPtr sptr 4
        lvals <- mapM (\x -> peek $ plusPtr lptr (x * 4)) [0..1023]
        rvals <- mapM (\x -> peek $ plusPtr rptr (x * 4)) [0..1023]
        _ <- takeMVar mv
        putMVar mv (lvals,rvals)

startAudio :: IO (MVar [IO ()])
startAudio = do
        mv <- newMVar []
        captureHandle <- audioInitialization
        _ <- forkIO $ audioLoop captureHandle mv
        return mv

audioLoop :: Ptr CInt -> MVar [IO ()] -> IO ()
audioLoop captureHandle mv = do
        processAudio_c captureHandle
        readMVar mv >>= mapM_ (\x -> x)
        audioLoop captureHandle mv
