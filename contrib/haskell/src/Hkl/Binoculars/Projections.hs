{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Projections
  ( AttenuationPath(..)
  , DataFrameHkl(..)
  , DataFrameQxQyQz(..)
  , DetectorPath(..)
  , GeometryPath(..)
  , HklPath(..)
  , HklBinocularsException(..)
  , QxQyQzPath(..)
  , SamplePath(..)
  , badAttenuation
  , saveCube
  , spaceHkl
  , spaceQxQyQz
  ) where

import           Control.Exception               (Exception)
import           Control.Monad                   (zipWithM)
import           Data.Array.Repa                 (Array, extent, listOfShape,
                                                  size)
import           Data.Array.Repa.Index           (DIM1, DIM2, DIM3, Z)
import           Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr)
import           Data.Text                       (Text)
import           Data.Typeable                   (typeOf)
import           Data.Word                       (Word16)
import           Foreign.C.String                (withCString)
import           Foreign.C.Types                 (CBool, CDouble (..),
                                                  CSize (..))
import           Foreign.ForeignPtr              (withForeignPtr)
import           Foreign.Marshal.Array           (withArrayLen)
import           Foreign.Ptr                     (Ptr, nullPtr)

import           Prelude                         hiding (drop)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.C.Sample
import           Hkl.Detector
import           Hkl.H5                          hiding (File)
import           Hkl.Image
import           Hkl.Orphan                      ()
import           Hkl.Types

--  Common

withNPixels :: Detector a DIM2 -> (CSize -> IO r) -> IO r
withNPixels d f = f (toEnum . size . shape $ d)

withPixelsDims :: Array F DIM3 Double -> (Int -> Ptr CSize -> IO r) -> IO r
withPixelsDims p = withArrayLen (map toEnum $ listOfShape . extent $ p)

saveCube :: FilePath -> [Cube DIM3] -> IO ()
saveCube o rs = do
  let c = (mconcat rs)
  case c of
    (Cube fp) ->
        withCString o $ \fn ->
        withForeignPtr fp $ \p ->
            c'hkl_binoculars_cube_save_hdf5 fn p
    EmptyCube -> return ()

withMaybeMask :: Maybe Mask -> (Ptr CBool -> IO r) -> IO r
withMaybeMask mm f = case mm of
                       Nothing  -> f nullPtr
                       (Just m) -> withForeignPtr (toForeignPtr m) $ \ptr -> f ptr

withMaybeLimits :: Maybe [Limits]
                -> Resolutions
                -> (Int -> Ptr (Ptr C'HklBinocularsAxisLimits) -> IO r)
                -> IO r
withMaybeLimits mls rs f = case mls of
                             Nothing   -> f 0 nullPtr
                             (Just ls) -> do
                                      ptrs <- zipWithM newLimits ls rs
                                      withForeignPtrs ptrs $ \pts ->
                                          withArrayLen pts f

-- DetectorPath

newtype DetectorPath = DetectorPath
    { detectorPathImage    :: Hdf5Path DIM3 Word16
    } deriving Show

-- GeometryPath

data GeometryPath
  = GeometryPathCristalK6C { geometryPathWavelength :: Hdf5Path Z Double
                           , geometryPathMu         :: Hdf5Path DIM1 Double
                           , geometryPathKomega     :: Hdf5Path DIM1 Double
                           , geometryPathKappa      :: Hdf5Path DIM1 Double
                           , geometryPathKphi       :: Hdf5Path DIM1 Double
                           , geometryPathGamma      :: Hdf5Path DIM1 Double
                           , geometryPathDelta      :: Hdf5Path DIM1 Double
                           }
  | GeometryPathFix { geometryPathWavelength :: Hdf5Path Z Double }
  | GeometryPathMars { geometryPathAxes       :: [Hdf5Path DIM1 Double]
                     }
  | GeometryPathMedH { geometryPathWavelength :: Hdf5Path Z Double
                     , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                     }
  | GeometryPathMedV { geometryPathWavelength :: Hdf5Path Z Double
                     , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                     }
  | GeometryPathMedVEiger { geometryPathWavelength :: Hdf5Path Z Double
                          , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                          , geometryPathEix        :: Hdf5Path DIM1 Double
                          , geometryPathEiz        :: Hdf5Path DIM1 Double
                          }
  | GeometryPathUhv { geometryPathWavelength :: Hdf5Path Z Double
                    , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                    }
                  deriving Show

-- AttenuationPath

data HklBinocularsException
    = WrongAttenuation Text Int Double
    deriving (Show)
instance Exception HklBinocularsException

data AttenuationPath
    = AttenuationPath { attenuationPath        :: Hdf5Path DIM1 Float
                      , attenuationOffset      :: Int
                      , attenuationCoefficient :: Double
                      }
    | ApplyedAttenuationFactorPath { attenuationPath :: Hdf5Path DIM1 Float }
    | NoAttenuation
    deriving Show

badAttenuation :: Float
badAttenuation = -100

--  QxQyQz Projection

data QxQyQzPath = QxQyQzPath AttenuationPath DetectorPath GeometryPath

instance Show QxQyQzPath where
  show = show . typeOf

type Resolutions = [Double]

data DataFrameQxQyQz
    = DataFrameQxQyQz
      Int -- n
      Double -- attenuation
      Geometry -- geometry
      Image -- image
    deriving Show

{-# INLINE spaceQxQyQz #-}
spaceQxQyQz :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> SurfaceOrientation -> Maybe [Limits] -> Space DIM3 -> DataFrameQxQyQz -> IO (DataFrameSpace DIM3)
spaceQxQyQz det pixels rs mmask' surf mlimits space (DataFrameQxQyQz _ att g img) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withArrayLen rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr (spaceHklPointer space) $ \pSpace -> do
  case img of
    (ImageInt32 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_q_int32_t" #-} c'hkl_binoculars_space_q_int32_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord16 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_q_uint16_t" #-} c'hkl_binoculars_space_q_uint16_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord32 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_q_uint32_t" #-} c'hkl_binoculars_space_q_uint32_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)

  return (DataFrameSpace img space att)

-- SamplePath

data SamplePath
    = SamplePath
      (Hdf5Path Z Double) -- a
      (Hdf5Path Z Double) -- b
      (Hdf5Path Z Double) -- c
      (Hdf5Path Z Double) -- alpha
      (Hdf5Path Z Double) -- beta
      (Hdf5Path Z Double) -- gamma
      (Hdf5Path Z Double) -- ux
      (Hdf5Path Z Double) -- uy
      (Hdf5Path Z Double) -- yz
    | SamplePath2 (Sample Triclinic)
    deriving Show

--  Hkl Projection

data HklPath = HklPath QxQyQzPath SamplePath
               deriving Show

data DataFrameHkl a
    = DataFrameHkl DataFrameQxQyQz (Sample Triclinic)
      deriving Show

{-# INLINE spaceHkl #-}
spaceHkl :: BinocularsConfig -> Detector b DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> Maybe [Limits] -> Space DIM3 -> DataFrameHkl b -> IO (DataFrameSpace DIM3)
spaceHkl config' det pixels rs mmask' mlimits space (DataFrameHkl (DataFrameQxQyQz _ att g img) samp) = do
  let sample' = overloadSampleWithConfig config' samp
  withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withSample sample' $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withMaybeMask mmask' $ \ mask'' ->
    withPixelsDims pixels $ \ndim dims ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withForeignPtr (spaceHklPointer space) $ \pSpace -> do
    case img of
      (ImageInt32 fp) -> withForeignPtr fp $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_int32_t" #-} c'hkl_binoculars_space_hkl_int32_t pSpace geometry sample i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
      (ImageWord16 fp) -> withForeignPtr fp $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint16_t" #-} c'hkl_binoculars_space_hkl_uint16_t pSpace geometry sample i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
      (ImageWord32 fp) -> withForeignPtr fp $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint32_t" #-} c'hkl_binoculars_space_hkl_uint32_t pSpace geometry sample i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
    return (DataFrameSpace img space att)
