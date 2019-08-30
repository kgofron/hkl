{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeInType               #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}

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
       ( Cube(..)
       , Space(..)
       , hkl_binoculars_cube_new
       , hkl_binoculars_space_q
       ) where

import           Data.Array.Repa.Repr.ForeignPtr (Array, F, fromForeignPtr)
import           Data.Array.Repa       (Shape, shapeOfList, showShape, extent)
import           Data.Word             (Word16)
import           Foreign.C.Types       (CDouble, CInt(..))
import           Foreign.Marshal.Array (peekArray)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, newForeignPtr_)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (Storable (..))
import           Hkl.C.Geometry

#include "hkl-binoculars.h"

data Cube sh = Cube { cubePhotons :: (Array F sh CInt)
                    , cubeContributions :: (Array F sh CInt)
                    , cubeHklPointer :: (ForeignPtr (Cube sh))
                    }
             deriving Show

instance Shape sh => Show (Array F sh CInt) where
  show = showShape . extent

instance Shape sh => Storable (Cube sh) where
  alignment _ = #{alignment HklBinocularsCube}
  sizeOf _ = #{size HklBinocularsCube}
  poke _ _ = undefined
  peek ptr = do
    n <- #{peek HklBinocularsCube, ndim} ptr :: IO CInt
    dims <- peekArray (fromEnum n) =<< (#{peek HklBinocularsCube, dims} ptr) :: IO [CInt]
    fpPhotons <- newForeignPtr_ =<< (#{peek HklBinocularsCube, photons} ptr)
    fpContributions <- newForeignPtr_ =<< (#{peek HklBinocularsCube, contributions} ptr)
    fp <- newForeignPtr hkl_binoculars_cube_free ptr
    let sh = shapeOfList (reverse (map fromEnum dims))
    return $ Cube (fromForeignPtr sh fpPhotons) (fromForeignPtr sh fpContributions) fp

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_cube_free" hkl_binoculars_cube_free :: FunPtr (Ptr (Cube sh) -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new" \
hkl_binoculars_cube_new :: CInt -- number of Space
                        -> Ptr (Ptr Space) -- spaces
                        -> CInt -- int32_t n_pixels
                        -> Ptr (Ptr Word16) -- uint16_t **imgs);
                        -> IO (Ptr (Cube sh))

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
    fp <- newForeignPtr hkl_binoculars_space_free ptr
    return $ Space n resolutions origins dims fp


foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_space_free" hkl_binoculars_space_free :: FunPtr (Ptr Space -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_q" \
hkl_binoculars_space_q :: Ptr Geometry -- const HklGeometry *geometry
                       -> Double -- double k
                       -> Ptr Word16 --  const uint16_t *image
                       -> CInt -- int32_t n_pixels
                       -> Ptr Double -- const double *pixels_coordinates
                       -> CInt -- int32_t pixels_coordinates_ndim
                       -> Ptr CInt --  const int32_t *pixels_coordinates_dims
                       -> Ptr Double --  const double *resolutions
                       -> CInt -- int32_t n_resolutions
                       -> IO (Ptr Space)
