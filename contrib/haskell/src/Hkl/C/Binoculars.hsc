#include "hkl-binoculars.h"
#include <bindings.dsl.h>

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE OverloadedStrings         #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.C.Binoculars
       ( C'HklBinocularsAxisLimits
       , Cube(..)
       , Space(..)
       , c'hkl_binoculars_axis_limits_free
       , c'hkl_binoculars_axis_limits_new
       , c'hkl_binoculars_cube_add_space
       , c'hkl_binoculars_cube_new
       , c'hkl_binoculars_cube_new_empty
       , c'hkl_binoculars_cube_new_empty_from_cube
       , c'hkl_binoculars_cube_save_hdf5
       , c'hkl_binoculars_space_hkl_int32_t
       , c'hkl_binoculars_space_hkl_uint16_t
       , c'hkl_binoculars_space_hkl_uint32_t
       , c'hkl_binoculars_space_q_int32_t
       , c'hkl_binoculars_space_q_uint16_t
       , c'hkl_binoculars_space_q_uint32_t
       , newCube
       , newLimits
       , newSpace
       , withForeignPtrs
       ) where

import           Data.Array.Repa       (Shape, size)
import           Data.Int              (Int32)
import           Data.Word             (Word16, Word32)
import           Foreign.C.Types       (CBool, CDouble(..), CInt(..), CSize(..), CPtrdiff)
import           Foreign.C.String      (CString)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, withForeignPtr)
import           Foreign.Ptr           (FunPtr, Ptr, nullPtr)
import           Foreign.Storable      (Storable (..))
import           System.IO.Unsafe      (unsafePerformIO)

import           Hkl.Binoculars.Config
import           Hkl.Detector
import           Hkl.C.Geometry
import           Hkl.C.Sample
import           Hkl.Orphan ()

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)


-- AxisLimits

#opaque_t HklBinocularsAxisLimits

newLimits :: Limits -> Double -> IO (ForeignPtr C'HklBinocularsAxisLimits)
newLimits (Limits mmin mmax) res =
    alloca $ \imin' ->
        alloca $ \imax' -> do
          imin'' <- case mmin of
                    Nothing -> pure nullPtr
                    (Just d) -> do
                              poke imin' (round (d / res))
                              pure imin'
          imax'' <- case mmax of
                    Nothing -> pure nullPtr
                    (Just d) -> do
                              poke imax' (round (d / res))
                              pure imax'
          newForeignPtr p'hkl_binoculars_axis_limits_free
                        =<< c'hkl_binoculars_axis_limits_new imin'' imax''

#ccall hkl_binoculars_axis_limits_free, \
  Ptr <HklBinocularsAxisLimits> -> IO ()

#ccall hkl_binoculars_axis_limits_new, \
  Ptr CPtrdiff -> Ptr CPtrdiff -> IO (Ptr <HklBinocularsAxisLimits>)

--  Cube

#opaque_t HklBinocularsCube

data Cube sh = Cube (ForeignPtr C'HklBinocularsCube)
             | EmptyCube
             deriving Show

newCube :: Ptr C'HklBinocularsCube -> IO (Cube sh)
newCube p = Cube <$> (newForeignPtr p'hkl_binoculars_cube_free p)

instance Shape sh => Semigroup (Cube sh) where
  {-# INLINE (<>) #-}
  EmptyCube <> a = a
  a <> EmptyCube = a
  (Cube fpa) <> (Cube fpb) = unsafePerformIO $ do
    withForeignPtr fpa $ \pa ->
      withForeignPtr fpb $ \pb ->
      newCube =<< {-# SCC "c'hkl_binoculars_cube_new_merge'" #-} c'hkl_binoculars_cube_new_merge pa pb

#ccall hkl_binoculars_cube_new_merge, \
  Ptr <HklBinocularsCube> -> Ptr <HklBinocularsCube> -> IO (Ptr <HklBinocularsCube>)

instance Shape sh => Monoid (Cube sh) where
  {-# INLINE mempty #-}
  mempty = EmptyCube

#ccall hkl_binoculars_cube_free, Ptr <HklBinocularsCube> -> IO ()

#ccall hkl_binoculars_cube_add_space, \
  Ptr <HklBinocularsCube> -> Ptr <HklBinocularsSpace> -> IO ()

#ccall hkl_binoculars_cube_new, \
  CSize -> Ptr (Ptr <HklBinocularsSpace>) -> IO (Ptr <HklBinocularsCube>)

#ccall hkl_binoculars_cube_new_empty, \
  IO (Ptr <HklBinocularsCube>)

#ccall hkl_binoculars_cube_new_empty_from_cube, \
  Ptr <HklBinocularsCube> -> IO (Ptr <HklBinocularsCube>)

#ccall hkl_binoculars_cube_save_hdf5, \
  CString -> Ptr <HklBinocularsCube> -> IO ()

--  Space

#opaque_t HklBinocularsSpace

newtype Space sh = Space (ForeignPtr C'HklBinocularsSpace)
  deriving Show

newSpace' :: Ptr C'HklBinocularsSpace -> IO (Space sh)
newSpace' p = Space <$> (newForeignPtr p'hkl_binoculars_space_free p)

newSpace :: (Shape sh1, Shape sh) => Detector a sh1 -> Int -> IO (Space sh)
newSpace d n = do
  let nPixels = toEnum . size . shape $ d
  let nDims = toEnum n
  newSpace' =<< c'hkl_binoculars_space_new nPixels nDims

#ccall hkl_binoculars_space_new, \
  CSize -> CSize -> IO (Ptr <HklBinocularsSpace>)

#ccall hkl_binoculars_space_free, \
  Ptr <HklBinocularsSpace> -> IO ()

type C'ProjectionTypeQ t = Ptr C'HklBinocularsSpace -- HklBinocularsSpace *self
 -> Ptr Geometry -- const HklGeometry *geometry
 -> Ptr t --  const uint16_t *image
 -> CSize -- size_t n_pixels
 -> CDouble -- double weight
 -> Ptr Double -- const double *pixels_coordinates
 -> CSize -- int32_t pixels_coordinates_ndim
 -> Ptr CSize --  const int32_t *pixels_coordinates_dims
 -> Ptr Double --  const double *resolutions
 -> CSize -- size_t n_resolutions
 -> Ptr CBool -- const uint8_t *mask
 -> CInt -- surface orientation
 -> Ptr (Ptr C'HklBinocularsAxisLimits) -- const HklBinocularsAxisLimits
 -> CInt -- size_t n_limits
 -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_q_int32_t" \
c'hkl_binoculars_space_q_int32_t :: C'ProjectionTypeQ Int32

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_q_uint16_t" \
c'hkl_binoculars_space_q_uint16_t :: C'ProjectionTypeQ Word16

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_q_uint32_t" \
c'hkl_binoculars_space_q_uint32_t :: C'ProjectionTypeQ Word32


type C'ProjectionTypeHkl t = Ptr C'HklBinocularsSpace -- HklBinocularsSpace *self
  -> Ptr Geometry -- const HklGeometry *geometry
  -> Ptr HklSample -- const HklSample *sample
  -> Ptr t --  const <t> *image
  -> CSize -- size_t n_pixels
  -> CDouble -- double weight
  -> Ptr Double -- const double *pixels_coordinates
  -> CSize -- size_t pixels_coordinates_ndim
  -> Ptr CSize --  const int32_t *pixels_coordinates_dims
  -> Ptr Double --  const double *resolutions
  -> CSize -- size_t n_resolutions
  -> Ptr CBool -- const uint8_t *mask
  -> Ptr (Ptr C'HklBinocularsAxisLimits) -- const HklBinocularsAxisLimits
  -> CInt -- size_t n_limits
  -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_int32_t" \
c'hkl_binoculars_space_hkl_int32_t :: C'ProjectionTypeHkl Int32

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_uint16_t" \
c'hkl_binoculars_space_hkl_uint16_t :: C'ProjectionTypeHkl Word16


foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_uint32_t" \
c'hkl_binoculars_space_hkl_uint32_t :: C'ProjectionTypeHkl Word32
