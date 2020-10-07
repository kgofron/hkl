{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE OverloadedStrings        #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
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
       , hkl_binoculars_cube_new_from_space
       , hkl_binoculars_space_q
       , hkl_binoculars_space_hkl
       , toCube
       ) where

import           Data.Array.Repa.Repr.ForeignPtr (Array, F, fromForeignPtr)
import           Data.Array.Repa       (DIM1, DIM3, Shape, shapeOfList, showShape, extent, ix1)
import           Data.ByteString.Char8 (ByteString, packCString)
import           Data.Word             (Word16)
import           Foreign.C.Types       (CDouble, CInt(..))
import           Foreign.Marshal.Alloc (finalizerFree)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (Storable (..))
import           System.IO.Unsafe      (unsafePerformIO)

import           Hkl.C.Geometry
import           Hkl.C.Sample
import           Hkl.H5

  #include "hkl-binoculars.h"

--  Axis

data Axis = Axis ByteString (Array F DIM1 CDouble)

instance Show Axis where
  show (Axis n _) = show n

instance Storable Axis where
  alignment _ = #{alignment HklBinocularsAxis}
  sizeOf _ = #{size HklBinocularsAxis}
  poke _ _ = undefined
  peek ptr = do
    n <- packCString =<< (#{peek HklBinocularsAxis, name} ptr)
    pArr <- hkl_binoculars_axis_array ptr
    fpArr <- newForeignPtr finalizerFree pArr
    return $ Axis n (fromForeignPtr (ix1 6) fpArr)

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
                                   -> CInt -- int32_t n_pixels
                                   -> Ptr Word16 -- uint16_t *imgs);
                                   -> IO (Ptr (Cube' sh))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new" \
hkl_binoculars_cube_new' :: CInt -- number of Space
                         -> Ptr (Ptr (Space sh)) -- spaces
                         -> CInt -- int32_t n_pixels
                         -> Ptr (Ptr Word16) -- uint16_t **imgs);
                         -> IO (Ptr (Cube' sh))

--  Cube

data Cube sh = Cube { cubePhotons :: (Array F sh CInt)
                    , cubeContributions :: (Array F sh CInt)
                    , cubeAxes :: [Axis]
                    , cubeHklPointer :: (ForeignPtr (Cube sh))
                    }
             | EmptyCube
             deriving Show

instance Shape sh => Show (Array F sh CInt) where
  show = showShape . extent

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
    n <- #{peek HklBinocularsCube, n_axes} ptr :: IO CInt
    allocaArray (fromEnum n) $ \dims' -> do
      hkl_binoculars_cube_dims ptr n dims'
      dims <- peekArray (fromEnum n) dims'
      let sh = shapeOfList (reverse (map fromEnum dims))
      fpPhotons <- newForeignPtr_ =<< (#{peek HklBinocularsCube, photons} ptr)
      fpContributions <- newForeignPtr_ =<< (#{peek HklBinocularsCube, contributions} ptr)
      axes <- peekArray (fromEnum n) =<< (#{peek HklBinocularsCube, axes} ptr)
      fp <- newForeignPtr hkl_binoculars_cube_free ptr
      return $ Cube (fromForeignPtr sh fpPhotons) (fromForeignPtr sh fpContributions) axes fp

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
  toHdf5 (Cube p c axes _) = group "binoculars"
                             [ group "axes" [dataset n arr | (Axis n arr) <- axes]
                             , dataset "counts" p
                             , dataset "contributions" c
                             ]
  toHdf5 EmptyCube = empty

toCube :: Shape sh => Cube' sh -> IO (Cube sh)
toCube EmptyCube' = pure EmptyCube
toCube (Cube' fp) = withForeignPtr fp $ \p -> do
  peek =<< hkl_binoculars_cube_new_copy p

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_cube_new_copy" \
hkl_binoculars_cube_new_copy :: Ptr (Cube' sh) -- src
                             -> IO (Ptr (Cube sh))

--  Space

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
                       -> Ptr Word16 --  const uint16_t *image
                       -> CInt -- int32_t n_pixels
                       -> Ptr Double -- const double *pixels_coordinates
                       -> CInt -- int32_t pixels_coordinates_ndim
                       -> Ptr CInt --  const int32_t *pixels_coordinates_dims
                       -> Ptr Double --  const double *resolutions
                       -> CInt -- int32_t n_resolutions
                       -> IO (Ptr (Space DIM3))

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl" \
hkl_binoculars_space_hkl :: Ptr Geometry -- const HklGeometry *geometry
                         -> Ptr HklSample -- const HklSample *sample
                         -> Ptr Word16 --  const uint16_t *image
                         -> CInt -- int32_t n_pixels
                         -> Ptr Double -- const double *pixels_coordinates
                         -> CInt -- int32_t pixels_coordinates_ndim
                         -> Ptr CInt --  const int32_t *pixels_coordinates_dims
                         -> Ptr Double --  const double *resolutions
                         -> CInt -- int32_t n_resolutions
                         -> IO (Ptr (Space DIM3))
