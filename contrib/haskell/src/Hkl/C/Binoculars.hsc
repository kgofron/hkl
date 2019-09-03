{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE OverloadedStrings        #-}

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
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, newForeignPtr_)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (Storable (..))
import           Hkl.C.Geometry
import           Hkl.H5

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
    allocaArray (fromEnum n) $ \dims' -> do
      hkl_binoculars_cube_dims ptr n dims'
      dims <- peekArray (fromEnum n) dims'
      let sh = shapeOfList (reverse (map fromEnum dims))
      fpPhotons <- newForeignPtr_ =<< (#{peek HklBinocularsCube, photons} ptr)
      fpContributions <- newForeignPtr_ =<< (#{peek HklBinocularsCube, contributions} ptr)
      fp <- newForeignPtr hkl_binoculars_cube_free ptr
      return $ Cube (fromForeignPtr sh fpPhotons) (fromForeignPtr sh fpContributions) fp

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_cube_free" hkl_binoculars_cube_free :: FunPtr (Ptr (Cube sh) -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new" \
hkl_binoculars_cube_new :: CInt -- number of Space
                        -> Ptr (Ptr (Space sh)) -- spaces
                        -> CInt -- int32_t n_pixels
                        -> Ptr (Ptr Word16) -- uint16_t **imgs);
                        -> IO (Ptr (Cube sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_dims" \
hkl_binoculars_cube_dims :: Ptr (Cube sh)
                         -> CInt
                         -> Ptr CInt
                         -> IO ()

instance Shape sh => ToHdf5 (Cube sh) where
  toHdf5 (Cube p c _) = group "binoculars" [ dataset "counts" p
                                           , dataset "contributions" c
                                           ]

data Space sh = Space { spaceHklPointer :: (ForeignPtr (Space sh)) }
  deriving Show

instance Shape sh => Storable (Space sh) where
  alignment _ = #{alignment HklBinocularsSpace}
  sizeOf _ = #{size HklBinocularsSpace}
  poke _ _ = undefined
  peek ptr = Space <$> newForeignPtr hkl_binoculars_space_free ptr


foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_space_free" hkl_binoculars_space_free :: FunPtr (Ptr (Space sh) -> IO ())

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
                       -> IO (Ptr (Space sh))
