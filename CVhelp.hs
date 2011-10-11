{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES cv_help.c #-}

module CVhelp where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List
import GHC.Conc (threadDelay)

foreign import ccall safe "cv_help.h open_video"
  open_video :: CString -> IO ()

foreign import ccall safe "cv_help.h advance"
  advance :: IO ()

foreign import ccall safe "cv_help.h close_video"
  close_video :: IO ()

foreign import ccall safe "cv_help.h pixel_value"
  pixel_value :: CInt -> CInt -> CUChar -> CUChar 

