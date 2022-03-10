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
  , DetectorPath(..)
  , GeometryPath(..)
  , HklBinocularsException(..)
  , badAttenuation
  , saveCube
  , withGeometry
  , withMaybeMask
  , withNPixels
  , withPixelsDims
  ) where

import           Control.Exception               (Exception)
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  listOfShape, size)
import           Data.Array.Repa.Index           (DIM1, DIM2, DIM3, Z)
import           Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr)
import           Data.Text                       (Text)
import           Data.Word                       (Word16)
import           Foreign.C.String                (withCString)
import           Foreign.C.Types                 (CBool, CSize (..))
import           Foreign.ForeignPtr              (withForeignPtr)
import           Foreign.Marshal.Array           (withArrayLen)
import           Foreign.Ptr                     (Ptr, nullPtr)

import           Prelude                         hiding (drop)

import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5                          hiding (File)
import           Hkl.Orphan                      ()

--  Common

withNPixels :: Detector a DIM2 -> (CSize -> IO r) -> IO r
withNPixels d f = f (toEnum . size . shape $ d)

withPixelsDims :: Array F DIM3 Double -> (Int -> Ptr CSize -> IO r) -> IO r
withPixelsDims p = withArrayLen (map toEnum $ listOfShape . extent $ p)

saveCube :: Shape sh => FilePath -> [Cube sh] -> IO ()
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

-- DetectorPath

newtype DetectorPath = DetectorPath
    { detectorPathImage    :: Hdf5Path DIM3 Word16
    } deriving (Eq, Show)

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
  | GeometryPathUhvTest { geometryPathWavelengthTest :: Angstrom
                        , geometryPathAxes           :: [Hdf5Path DIM1 Double]
                        }
                  deriving (Eq, Show)

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
    deriving (Eq, Show)

badAttenuation :: Float
badAttenuation = -100
