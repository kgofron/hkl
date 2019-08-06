{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.C.Binoculars
       ( Space(..)
       , c_hkl_binoculars_project_q
       , c_hkl_binoculars_space_free
       , c_hkl_binoculars_space_from_image
       ) where

import           Data.Word             (Word16)
import           Foreign.C.Types       (CDouble, CInt(..))
import           Foreign.Marshal.Array (peekArray)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (Storable (..))

import           Hkl.C.Geometry

#include "hkl-binoculars.h"

data Space = Space CInt [CDouble] [CInt] [CInt] (ForeignPtr Space)
  deriving Show

instance Storable Space where
  alignment _ = #{alignment HklBinocularsSpace}
  sizeOf _ = #{size HklBinocularsSpace}
  poke _ _ = undefined
  peek ptr = do
    n <- #{peek HklBinocularsSpace, ndim} ptr
    resolutions <- peekArray (fromEnum n) =<< (#{peek HklBinocularsSpace, resolutions} ptr)
    origins <- peekArray (fromEnum n) =<< (#{peek HklBinocularsSpace, origin} ptr)
    dims <- peekArray (fromEnum n) =<< (#{peek HklBinocularsSpace, dims} ptr)
    fp <- newForeignPtr c_hkl_binoculars_space_free ptr
    return $ Space n resolutions origins dims fp


foreign import ccall unsafe "hkl_binoculars_project_q" c_hkl_binoculars_project_q :: Ptr Geometry -> Ptr Double -> CInt -> Double -> IO (Ptr Double)

foreign import ccall unsafe "hkl_binoculars_space_from_image" c_hkl_binoculars_space_from_image :: Ptr CDouble -> CInt -> Ptr Double -> Ptr CInt -> CInt -> Ptr Word16 -> CInt -> IO (Ptr Space)

foreign import ccall unsafe "&hkl_binoculars_space_free" c_hkl_binoculars_space_free :: FunPtr (Ptr Space -> IO ())
