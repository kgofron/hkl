{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE OverloadedStrings        #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}

{-
    Copyright  : Copyright (C) 2014-2021 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.C.Binoculars
       ( Cube(..)
       , Cube'(..)
       , Space(..)
       , hkl_binoculars_cube_new
       , hkl_binoculars_cube_new'
       , hkl_binoculars_cube_new_empty'
       , hkl_binoculars_cube_new_from_space
       , hkl_binoculars_cube_add_space
       , hkl_binoculars_space_q_uint16_t
       , hkl_binoculars_space_hkl_uint16_t
       , newSpace
       , toCube
       ) where

import           Data.Array.Repa.Repr.ForeignPtr (Array, F, fromForeignPtr)
import           Data.Array.Repa       (DIM1, DIM3, Shape, shapeOfList, ix1, size)
import           Data.ByteString.Char8 (ByteString, packCString)
import           Data.Word             (Word16)
import           Foreign.C.Types       (CBool, CDouble(..), CSize(..), CUInt(..), CPtrdiff)
import           Foreign.Marshal.Alloc (finalizerFree)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import           Foreign.Ptr           (FunPtr, Ptr, plusPtr)
import           Foreign.Storable      (Storable (..))
import           System.IO.Unsafe      (unsafePerformIO)

import           Hkl.Detector
import           Hkl.C.Geometry
import           Hkl.C.Sample
import           Hkl.H5
import           Hkl.Orphan ()

#include "hkl-binoculars.h"

--  Axis

data Axis = Axis { name :: ByteString
                 , index :: CSize
                 , resolution :: CDouble
                 , imin :: CPtrdiff
                 , imax :: CPtrdiff
                 , arr :: Array F DIM1 CDouble
                 } deriving Show

instance Storable Axis where
  alignment _ = #{alignment HklBinocularsAxis}
  sizeOf _ = #{size HklBinocularsAxis}
  poke _ _ = undefined
  peek ptr = Axis
             <$> (packCString =<< (#{peek HklBinocularsAxis, name} ptr))
             <*> #{peek HklBinocularsAxis, index} ptr
             <*> #{peek HklBinocularsAxis, resolution} ptr
             <*> #{peek HklBinocularsAxis, imin} ptr
             <*> #{peek HklBinocularsAxis, imax} ptr
             <*> (fromForeignPtr (ix1 6)
                 <$> (newForeignPtr finalizerFree =<< hkl_binoculars_axis_array ptr))


foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_axis_array" \
hkl_binoculars_axis_array :: Ptr Axis -> IO (Ptr CDouble)

--  Cube'

data Cube' sh = Cube' (ForeignPtr (Cube' sh))
              | EmptyCube'
              deriving Show

instance Shape sh => Semigroup (Cube' sh) where
  {-# INLINE (<>) #-}
  EmptyCube' <> a = a
  a <> EmptyCube' = a
  (Cube' fpa) <> (Cube' fpb) = unsafePerformIO $ do
    withForeignPtr fpa $ \pa ->
      withForeignPtr fpb $ \pb ->
      peek =<< {-# SCC "hkl_binoculars_cube_new_merge'" #-} hkl_binoculars_cube_new_merge' pa pb


foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_merge" \
hkl_binoculars_cube_new_merge' :: Ptr (Cube' sh)
                               -> Ptr (Cube' sh)
                               -> IO (Ptr (Cube' sh))


instance Shape sh => Monoid (Cube' sh) where
  {-# INLINE mempty #-}
  mempty = EmptyCube'

instance Shape sh => Storable (Cube' sh) where
  alignment _ = #{alignment HklBinocularsCube}
  sizeOf _ = #{size HklBinocularsCube}
  poke _ _ = undefined
  {-# INLINE peek #-}
  peek ptr = Cube' <$> newForeignPtr hkl_binoculars_cube_free' ptr

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_cube_free" hkl_binoculars_cube_free' :: FunPtr (Ptr (Cube' sh) -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_from_space" \
hkl_binoculars_cube_new_from_space :: Ptr (Space sh) -- space
                                   -> IO (Ptr (Cube' sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_add_space" \
hkl_binoculars_cube_add_space :: Ptr (Cube' sh) -- HklBinocularsCube *self
                              -> Ptr (Space sh) -- const HklBinocularsSpace *space
                              -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new" \
hkl_binoculars_cube_new' :: CSize -- size_t n_spaces
                         -> Ptr (Ptr (Space sh)) -- HklBinocularsSpace **spaces
                         -> IO (Ptr (Cube' sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_empty" \
hkl_binoculars_cube_new_empty' :: IO (Ptr (Cube' sh))

--  Cube

data Cube sh = Cube { cubePhotons :: (Array F sh CUInt)
                    , cubeContributions :: (Array F sh CUInt)
                    , cubeAxes :: [Axis]
                    , cubeHklPointer :: (ForeignPtr (Cube sh))
                    }
             | EmptyCube
             deriving Show

instance Shape sh => Semigroup (Cube sh) where
  (<>) EmptyCube EmptyCube = EmptyCube
  (<>) EmptyCube b = b
  (<>) a EmptyCube = a
  (<>) (Cube _ _ _ fpa) (Cube _ _ _ fpb) = unsafePerformIO $ do
    withForeignPtr fpa $ \pa ->
      withForeignPtr fpb $ \pb ->
      peek =<< {-# SCC "hkl_binoculars_cube_new_merge" #-} hkl_binoculars_cube_new_merge pa pb

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_merge" \
hkl_binoculars_cube_new_merge :: Ptr (Cube sh)
                              -> Ptr (Cube sh)
                              -> IO (Ptr (Cube sh))

instance Shape sh => Monoid (Cube sh) where
  mempty = EmptyCube

instance Shape sh => Storable (Cube sh) where
  alignment _ = #{alignment HklBinocularsCube}
  sizeOf _ = #{size HklBinocularsCube}
  poke _ _ = undefined
  {-# INLINE peek #-}
  peek ptr = do
    let paxes = #{ptr HklBinocularsCube, axes} ptr
    n <- #{peek darray_axis, size} paxes
    allocaArray (fromEnum n) $ \dims' -> do
      hkl_binoculars_cube_dims ptr n dims'
      dims <- peekArray (fromEnum n) dims'
      let sh = shapeOfList (reverse (map fromEnum dims))
      fpPhotons <- newForeignPtr_ =<< (#{peek HklBinocularsCube, photons} ptr)
      fpContributions <- newForeignPtr_ =<< (#{peek HklBinocularsCube, contributions} ptr)
      axes <- peekArray (fromEnum n) =<< (#{peek darray_axis, item} paxes)
      fp <- newForeignPtr hkl_binoculars_cube_free ptr
      return $ Cube (fromForeignPtr sh fpPhotons) (fromForeignPtr sh fpContributions) axes fp

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_cube_free" hkl_binoculars_cube_free :: FunPtr (Ptr (Cube sh) -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new" \
hkl_binoculars_cube_new :: CSize -- number of Space
                        -> Ptr (Ptr (Space sh)) -- spaces
                        -> IO (Ptr (Cube sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_dims" \
hkl_binoculars_cube_dims :: Ptr (Cube sh)
                         -> CSize
                         -> Ptr CSize
                         -> IO ()

instance Shape sh => ToHdf5 (Cube sh) where
  toHdf5 (Cube p c axes _) = group "binoculars"
                             [ group "axes" [dataset (name axis) (arr axis) | axis <- axes]
                             , dataset "counts" p
                             , dataset "contributions" c
                             ]
  toHdf5 EmptyCube = empty

toCube :: Shape sh => Cube' sh -> IO (Cube sh)
toCube EmptyCube' = pure EmptyCube
toCube (Cube' fp') = withForeignPtr fp' $ \p -> do
  peek =<< hkl_binoculars_cube_new_copy p

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_copy" \
hkl_binoculars_cube_new_copy :: Ptr (Cube' sh) -- src
                             -> IO (Ptr (Cube sh))

--  Space

newtype Space sh = Space { spaceHklPointer :: (ForeignPtr (Space sh)) }
  deriving Show

instance Shape sh => Storable (Space sh) where
  alignment _ = #{alignment HklBinocularsSpace}
  sizeOf _ = #{size HklBinocularsSpace}
  poke _ _ = undefined
  peek ptr = Space <$> newForeignPtr hkl_binoculars_space_free ptr

newSpace :: (Shape sh1, Shape sh) => Detector a sh1 -> Int -> IO (Space sh)
newSpace d n = do
  let nPixels = toEnum . size . shape $ d
  let nDims = toEnum n
  ptr <- hkl_binoculars_space_new nPixels nDims
  Space <$> newForeignPtr hkl_binoculars_space_free ptr

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_new" \
hkl_binoculars_space_new :: CSize -- size_t n_index0 (aka n_pixels)
                         -> CSize -- size_t n_axes
                         -> IO (Ptr (Space sh))

foreign import ccall unsafe "hkl-binoculars.h &hkl_binoculars_space_free" hkl_binoculars_space_free :: FunPtr (Ptr (Space sh) -> IO ())

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_q_uint16_t" \
hkl_binoculars_space_q_uint16_t :: Ptr (Space DIM3) -- HklBinocularsSpace *self
                                -> Ptr Geometry -- const HklGeometry *geometry
                                -> Ptr Word16 --  const uint16_t *image
                                -> CSize -- size_t n_pixels
                                -> CDouble -- double weight
                                -> Ptr Double -- const double *pixels_coordinates
                                -> CSize -- int32_t pixels_coordinates_ndim
                                -> Ptr CSize --  const int32_t *pixels_coordinates_dims
                                -> Ptr Double --  const double *resolutions
                                -> CSize -- size_t n_resolutions
                                -> Ptr CBool -- const uint8_t *mask
                                -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_uint16_t" \
hkl_binoculars_space_hkl_uint16_t :: Ptr (Space DIM3) -- HklBinocularsSpace *self
                                  -> Ptr Geometry -- const HklGeometry *geometry
                                  -> Ptr HklSample -- const HklSample *sample
                                  -> Ptr Word16 --  const uint16_t *image
                                  -> CSize -- size_t n_pixels
                                  -> CDouble -- double weight
                                  -> Ptr Double -- const double *pixels_coordinates
                                  -> CSize -- size_t pixels_coordinates_ndim
                                  -> Ptr CSize --  const int32_t *pixels_coordinates_dims
                                  -> Ptr Double --  const double *resolutions
                                  -> CSize -- size_t n_resolutions
                                  -> Ptr CBool -- const uint8_t *mask
                                  -> IO ()
