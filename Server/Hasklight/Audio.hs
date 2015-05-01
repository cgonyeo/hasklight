{-# LANGUAGE ForeignFunctionInterface #-}
module Hasklight.Audio where

import Control.Concurrent
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Hasklight.LED

foreign import ccall "audioInitialization" audioInitialization :: IO ()
foreign import ccall "getSoundBuffer" getSoundBuffer :: IO (Ptr (Ptr CFloat))
foreign import ccall "getFFTBuffer" getFFTBuffer :: IO (Ptr (Ptr CFloat))
foreign import ccall "getOnBeat" getOnBeat_c :: IO CInt

startAudio :: MVar Int -> IO ()
startAudio m = audioInitialization >> putMVar m 1

getSound :: IO ([Float],[Float])
getSound = do
        sptr <- getSoundBuffer
        lptr <- peek sptr
        rptr <- peek $ plusPtr sptr 4
        lvals <- mapM (\x -> peek $ plusPtr lptr (x * 4)) [0..1023]
        rvals <- mapM (\x -> peek $ plusPtr rptr (x * 4)) [0..1023]
        return (lvals,rvals)

getFFT :: IO ([Float],[Float])
getFFT = do
        sptr <- getFFTBuffer
        lptr <- peek sptr
        rptr <- peek $ plusPtr sptr 4
        lvals <- mapM (\x -> peek $ plusPtr lptr (x * 4)) [0..1023]
        rvals <- mapM (\x -> peek $ plusPtr rptr (x * 4)) [0..1023]
        return (lvals,rvals)

getOnBeat :: IO Bool
getOnBeat = do
        onBeat <- getOnBeat_c
        return (onBeat /= 0)
