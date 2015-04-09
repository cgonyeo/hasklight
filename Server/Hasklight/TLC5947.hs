{-# LANGUAGE ForeignFunctionInterface #-}
module Hasklight.TLC5947 where

import Foreign.C

foreign import ccall "tlcSetLed" tlcSetLed_c :: CInt -> CInt -> IO ()
foreign import ccall "tlcClearLeds" tlcClearLeds :: IO ()
foreign import ccall "tlc5947init" tlcInit :: IO ()
foreign import ccall "tlc5947cleanup" tlcCleanup :: IO ()
foreign import ccall "tlcUpdateLeds" tlcUpdateLeds :: IO ()

tlcSetLed :: Int -> Int -> IO ()
tlcSetLed n1 n2 = tlcSetLed_c (fromIntegral n1) (fromIntegral n2)
