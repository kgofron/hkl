{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
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
  , InputHkl(..)
  , InputQxQyQz(..)
  , QxQyQzPath(..)
  , SamplePath(..)
  , badAttenuation
  , saveCube
  , spaceHkl
  , spaceQxQyQz
  ) where

import           Data.Array.Repa                   (Array, extent, listOfShape,
                                                    size)
import           Data.Array.Repa.Index             (DIM1, DIM2, DIM3, Z)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Maybe                        (fromJust)
import           Data.Typeable                     (typeOf)
import           Data.Word                         (Word16)
import           Foreign.C.Types                   (CBool, CSize (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr, nullPtr)
import           Numeric.Units.Dimensional.Prelude (Angle, Length)

import           Prelude                           hiding (drop, mapM)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.C.Sample
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Orphan                        ()
import           Hkl.Types

--  Common

withNPixels :: Detector a DIM2 -> (CSize -> IO r) -> IO r
withNPixels d f = f (toEnum . size . shape $ d)

withPixelsDims :: Array F DIM3 Double -> (Int -> Ptr CSize -> IO r) -> IO r
withPixelsDims p = withArrayLen (map toEnum $ listOfShape . extent $ p)

saveCube :: FilePath -> [Cube' DIM3] -> IO ()
saveCube o rs = saveHdf5 o =<< toCube (mconcat rs)

withMaybeMask :: Maybe Mask -> (Ptr CBool -> IO r) -> IO r
withMaybeMask mm f = case mm of
                       Nothing  -> f nullPtr
                       (Just m) -> withForeignPtr (toForeignPtr m) $ \ptr -> f ptr

-- DetectorPath

newtype DetectorPath = DetectorPath
    { detectorPathImage    :: Hdf5Path DIM3 Word16
    } deriving Show

-- GeometryPath

data GeometryPath
  = GeometryPathUhv { geometryPathWavelength :: Hdf5Path Z Double
                    , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                    }
  | GeometryPathMedV { geometryPathWavelength :: Hdf5Path Z Double
                     , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                     }
  | GeometryPathCristalK6C { geometryPathWavelength :: Hdf5Path Z Double
                           , geometryPathMu         :: Hdf5Path DIM1 Double
                           , geometryPathKomega     :: Hdf5Path DIM1 Double
                           , geometryPathKappa      :: Hdf5Path DIM1 Double
                           , geometryPathKphi       :: Hdf5Path DIM1 Double
                           , geometryPathGamma      :: Hdf5Path DIM1 Double
                           , geometryPathDelta      :: Hdf5Path DIM1 Double
                           }

                  deriving Show

-- AttenuationPath

data AttenuationPath
    = AttenuationPath { attenuationPath        :: Hdf5Path DIM1 Float
                      , attenuationOffset      :: Int
                      , attenuationCoefficient :: Double
                      }
    | NoAttenuation
    deriving Show

badAttenuation :: Float
badAttenuation = -100

--  QxQyQz Projection

data QxQyQzPath = QxQyQzPath DetectorPath GeometryPath AttenuationPath

instance Show QxQyQzPath where
  show = show . typeOf

type Resolutions = [Double]

data InputQxQyQz a =
  InputQxQyQz { detector     :: Detector Hkl DIM2
              , filename     :: InputFn
              , h5dpath      :: a
              , output       :: FilePath
              , resolutions  :: [Double]
              , centralPixel :: (Int, Int)  -- x, y
              , sdd'         :: Length Double  -- sample to detector distance
              , detrot'      :: Angle Double
              , mask         :: Maybe Mask
              }
  deriving Show

data DataFrameQxQyQz
    = DataFrameQxQyQz
      Int -- n
      Geometry -- geometry
      (ForeignPtr Word16) -- image
      (Maybe Double) -- attenuation
    deriving Show

{-# INLINE spaceQxQyQz #-}
spaceQxQyQz :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> Space DIM3 -> DataFrameQxQyQz -> IO (DataFrameSpace DIM3)
spaceQxQyQz det pixels rs mmask' space (DataFrameQxQyQz _ g img matt) =
  withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withPixelsDims pixels $ \ndim dims ->
    withMaybeMask mmask' $ \ mask'' ->
    withForeignPtr img $ \i ->
    withForeignPtr (spaceHklPointer space) $ \pSpace -> do
      {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_q pSpace geometry i nPixels pix (toEnum ndim) dims r (toEnum nr) mask''
      return (DataFrameSpace img space (fromJust matt))

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

data InputHkl a =
  InputHkl { detector     :: Detector Hkl DIM2
           , filename     :: InputFn
           , h5dpath      :: a
           , output       :: FilePath
           , resolutions  :: [Double]
           , centralPixel :: (Int, Int)  -- x, y
           , sdd'         :: Length Double  -- sample to detector distance
           , detrot'      :: Angle Double
           , config       :: BinocularsConfig
           , mask         :: Maybe Mask
           }
  deriving Show

data DataFrameHkl a
    = DataFrameHkl DataFrameQxQyQz (Sample Triclinic)
      deriving Show

{-# INLINE spaceHkl #-}
spaceHkl :: BinocularsConfig -> Detector b DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> Space DIM3 -> DataFrameHkl b -> IO (DataFrameSpace DIM3)
spaceHkl config' det pixels rs mmask' space (DataFrameHkl (DataFrameQxQyQz _ g img matt) samp) = do
  let sample' = overloadSampleWithConfig config' samp
  withNPixels det $ \nPixels ->
      withGeometry g $ \geometry ->
      withSample sample' $ \sample ->
      withForeignPtr (toForeignPtr pixels) $ \pix ->
      withArrayLen rs $ \nr r ->
      withMaybeMask mmask' $ \ mask'' ->
      withPixelsDims pixels $ \ndim dims ->
      withForeignPtr img $ \i ->
      withForeignPtr (spaceHklPointer space) $ \pSpace -> do
        {-# SCC "hkl_binoculars_space_hkl" #-} hkl_binoculars_space_hkl pSpace geometry sample i nPixels pix (toEnum ndim) dims r (toEnum nr) mask''
        return (DataFrameSpace img space (fromJust matt))
