{-# LANGUAGE ForeignFunctionInterface #-}
module Audio.Audio where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "audioInitialization" audioInitialization :: IO ()
foreign import ccall "getSoundBuffer" getSoundBuffer_c :: IO (Ptr CFloat)
foreign import ccall "runFFT" runFFT_c :: IO (Ptr CFloat)

getSoundBuffer :: IO [Float]
getSoundBuffer = do sptr <- getSoundBuffer_c
                    mapM (\x -> peek $ plusPtr sptr (x * 4)) [0..1023]

runFFT :: IO [Float]
runFFT = do sptr <- runFFT_c
            mapM (\x -> peek $ plusPtr sptr (x * 4)) [0..1023]
