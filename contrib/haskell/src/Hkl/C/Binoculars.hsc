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
       , c_hkl_binoculars_space_free
       , c_hkl_binoculars_space_q
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


foreign import ccall unsafe "&hkl_binoculars_space_free" c_hkl_binoculars_space_free :: FunPtr (Ptr Space -> IO ())

foreign import ccall unsafe "hkl_binoculars_space_q" \
c_hkl_binoculars_space_q :: Ptr Geometry -- const HklGeometry *geometry
                         -> Double -- double k
                         -> Ptr Word16 --  const uint16_t *image
                         -> CInt -- int32_t n_pixels
                         -> Ptr Double -- const double *pixels_coordinates
                         -> CInt -- int32_t pixels_coordinates_ndim
                         -> Ptr CInt --  const int32_t *pixels_coordinates_dims
                         -> Ptr Double --  const double *resolutions
                         -> CInt -- int32_t n_resolutions
                         -> IO (Ptr Space)
