{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}

module Hkl.C.Detector
       ( HklDetector
       , newDetector
       , withDetector
       ) where

import Prelude hiding (min, max)

import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , newForeignPtr
               , withForeignPtr)
import Foreign.C (CInt(..))

import Hkl.Detector

#include "hkl.h"

data HklDetector

-- Detector

withDetector :: Detector a -> (Ptr HklDetector -> IO b) -> IO b
withDetector d func = do
  fptr <- newDetector d
  withForeignPtr fptr func

newDetector :: Detector a -> IO (ForeignPtr HklDetector)
newDetector ZeroD = c_hkl_detector_factory_new 0 >>= newForeignPtr c_hkl_detector_free
newDetector _ = error "Can not use 2D detector with the hkl library"

foreign import ccall unsafe "hkl.h hkl_detector_factory_new"
  c_hkl_detector_factory_new:: CInt -> IO (Ptr HklDetector)

foreign import ccall unsafe "hkl.h &hkl_detector_free"
  c_hkl_detector_free :: FunPtr (Ptr HklDetector -> IO ())
