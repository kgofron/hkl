{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE OverloadedStrings        #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}

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
       , c'hkl_binoculars_cube_new_from_space
       , c'hkl_binoculars_cube_save_hdf5
       , c'hkl_binoculars_space_hkl_int32_t
       , c'hkl_binoculars_space_hkl_uint16_t
       , c'hkl_binoculars_space_hkl_uint32_t
       , c'hkl_binoculars_space_q_int32_t
       , c'hkl_binoculars_space_q_uint16_t
       , c'hkl_binoculars_space_q_uint32_t
       , newLimits
       , newSpace
       , withForeignPtrs
       ) where

import           Data.Array.Repa       (DIM3, Shape, size)
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

#include "hkl-binoculars.h"

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)


-- AxisLimits

data C'HklBinocularsAxisLimits

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
          newForeignPtr c'hkl_binoculars_axis_limits_free
                        =<< c'hkl_binoculars_axis_limits_new imin'' imax''

foreign import ccall unsafe "&hkl_binoculars_axis_limits_free" \
c'hkl_binoculars_axis_limits_free :: FunPtr (Ptr C'HklBinocularsAxisLimits -> IO ())

foreign import ccall unsafe "hkl_binoculars_axis_limits_new" \
c'hkl_binoculars_axis_limits_new :: Ptr CPtrdiff
                                 -> Ptr CPtrdiff
                                 -> IO (Ptr C'HklBinocularsAxisLimits)

--  Cube

data Cube sh = Cube (ForeignPtr (Cube sh))
              | EmptyCube
              deriving Show

instance Shape sh => Semigroup (Cube sh) where
  {-# INLINE (<>) #-}
  EmptyCube <> a = a
  a <> EmptyCube = a
  (Cube fpa) <> (Cube fpb) = unsafePerformIO $ do
    withForeignPtr fpa $ \pa ->
      withForeignPtr fpb $ \pb ->
      peek =<< {-# SCC "hkl_binoculars_cube_new_merge'" #-} c'hkl_binoculars_cube_new_merge pa pb


foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_merge" \
c'hkl_binoculars_cube_new_merge :: Ptr (Cube sh)
                               -> Ptr (Cube sh)
                               -> IO (Ptr (Cube sh))


instance Shape sh => Monoid (Cube sh) where
  {-# INLINE mempty #-}
  mempty = EmptyCube

instance Shape sh => Storable (Cube sh) where
  alignment _ = #{alignment HklBinocularsCube*}
  sizeOf _ = #{size HklBinocularsCube*}
  poke _ _ = undefined
  {-# INLINE peek #-}
  peek ptr = Cube <$> newForeignPtr c'hkl_binoculars_cube_free ptr

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_cube_free" \
c'hkl_binoculars_cube_free :: FunPtr (Ptr (Cube sh) -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_from_space" \
c'hkl_binoculars_cube_new_from_space :: Ptr (Space sh) -- space
                                   -> IO (Ptr (Cube sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_add_space" \
c'hkl_binoculars_cube_add_space :: Ptr (Cube sh) -- HklBinocularsCube *self
                              -> Ptr (Space sh) -- const HklBinocularsSpace *space
                              -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new" \
c'hkl_binoculars_cube_new :: CSize -- size_t n_spaces
                         -> Ptr (Ptr (Space sh)) -- HklBinocularsSpace **spaces
                         -> IO (Ptr (Cube sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_empty" \
c'hkl_binoculars_cube_new_empty :: IO (Ptr (Cube sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_empty_from_cube" \
c'hkl_binoculars_cube_new_empty_from_cube :: Ptr (Cube sh) -> IO (Ptr (Cube sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_save_hdf5" \
c'hkl_binoculars_cube_save_hdf5 :: CString -> Ptr (Cube sh) -> IO ()

--  Space

newtype Space sh = Space { spaceHklPointer :: (ForeignPtr (Space sh)) }
  deriving Show

instance Shape sh => Storable (Space sh) where
  alignment _ = #{alignment HklBinocularsSpace*}
  sizeOf _ = #{size HklBinocularsSpace*}
  poke _ _ = undefined
  peek ptr = Space <$> newForeignPtr c'hkl_binoculars_space_free ptr

newSpace :: (Shape sh1, Shape sh) => Detector a sh1 -> Int -> IO (Space sh)
newSpace d n = do
  let nPixels = toEnum . size . shape $ d
  let nDims = toEnum n
  ptr <- c'hkl_binoculars_space_new nPixels nDims
  Space <$> newForeignPtr c'hkl_binoculars_space_free ptr

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_new" \
c'hkl_binoculars_space_new :: CSize -- size_t n_index0 (aka n_pixels)
                         -> CSize -- size_t n_axes
                         -> IO (Ptr (Space sh))

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_space_free" \
c'hkl_binoculars_space_free :: FunPtr (Ptr (Space sh) -> IO ())

type C'ProjectionTypeQ t = Ptr (Space DIM3) -- HklBinocularsSpace *self
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


type C'ProjectionTypeHkl t = Ptr (Space DIM3) -- HklBinocularsSpace *self
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
