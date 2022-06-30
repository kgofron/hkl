{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.DataSource
  ( DataSource(..)
  , DataSourceAcq(..)
  , DataSourcePath(..)
  , HklBinocularsException(..)
  , Is0DStreamable(..)
  , Is1DStreamable(..)
  , withAttenuationPathP
  , withGeometryPathP
  , withDetectorPathP
  ) where

import           Bindings.HDF5.Core                (Location)
import           Bindings.HDF5.Dataset             (getDatasetType)
import           Bindings.HDF5.Datatype            (getTypeSize, nativeTypeOf,
                                                    typeIDsEqual)
import           Control.Exception                 (Exception, throwIO)
import           Control.Monad.Extra               (ifM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Data.Array.Repa                   (size)
import           Data.Array.Repa.Index             (DIM1, DIM2, DIM3, Z)
import           Data.Int                          (Int32)
import           Data.Text                         (Text)
import           Data.Vector.Storable              (Vector, fromList)
import           Data.Word                         (Word16, Word32)
import           GHC.Base                          (returnIO)
import           GHC.Float                         (float2Double)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (degree, (*~))
import           Pipes.Safe                        (MonadSafe)

import           Prelude                           hiding (filter)

import           Hkl.Binoculars.Config
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Types

--------------------
-- Is0DStreamable --
--------------------

class Is0DStreamable a e where
  extract0DStreamValue :: a -> IO e

instance Is0DStreamable Dataset Double where
  extract0DStreamValue d = get_position d 0

instance Is0DStreamable (DataSourceAcq Degree) Degree where
    extract0DStreamValue (DataSourceAcq'Degree d) =
        Degree <$> do
          v <- extract0DStreamValue d
          return $ v *~ degree

instance Is0DStreamable (DataSourceAcq Degree) Double where
  extract0DStreamValue (DataSourceAcq'Degree d) = extract0DStreamValue d

instance Is0DStreamable (DataSourceAcq NanoMeter) NanoMeter where
    extract0DStreamValue (DataSourceAcq'NanoMeter d) =
        NanoMeter <$> do
          v <- extract0DStreamValue d
          return $ v *~ angstrom

instance Is0DStreamable (DataSourceAcq WaveLength) WaveLength where
  extract0DStreamValue (DataSourceAcq'WaveLength d) = do
    v <- extract0DStreamValue d
    return $ v *~ angstrom
  extract0DStreamValue (DataSourceAcq'WaveLength'Const a) = return $ unAngstrom a

instance Is0DStreamable (DataSourceAcq WaveLength) Source where
  extract0DStreamValue d = Source <$> extract0DStreamValue d

--------------------
-- Is1DStreamable --
--------------------

class Is1DStreamable a e where
  extract1DStreamValue :: a -> Int -> IO e

-- Is1DStreamable (instances)

badAttenuation :: Float
badAttenuation = -100

instance Is1DStreamable (DataSourceAcq Attenuation) Attenuation where
    extract1DStreamValue (DataSourceAcq'Attenuation ds offset coef mmax) i =
        Attenuation <$> do
          v <-  extract1DStreamValue ds (i + offset)
          if v == badAttenuation
          then throwIO (WrongAttenuation "attenuation is wrong" (i + offset) (float2Double v))
          else case mmax of
                 Just m -> if v > m
                          then throwIO (WrongAttenuation "max inserted filters exceeded" (i + offset) (float2Double v))
                          else return (coef ** float2Double v)
                 Nothing -> return (coef ** float2Double v)

    extract1DStreamValue (DataSourceAcq'ApplyedAttenuationFactor ds) i = extract1DStreamValue ds i

    extract1DStreamValue DataSourceAcq'NoAttenuation _ = returnIO $ Attenuation 1


instance Is1DStreamable (DataSourceAcq Degree) Double where
  extract1DStreamValue (DataSourceAcq'Degree d) = extract1DStreamValue d

instance Is1DStreamable Dataset Double where
  extract1DStreamValue = get_position

instance Is1DStreamable Dataset Float where
  extract1DStreamValue = get_position

instance Is1DStreamable (DataSourceAcq Float) Float where
  extract1DStreamValue (DataSourceAcq'Float ds) = extract1DStreamValue ds

instance Is1DStreamable (DataSourceAcq Float) Attenuation where
  extract1DStreamValue (DataSourceAcq'Float ds) i = Attenuation <$> extract1DStreamValue ds i

instance Is1DStreamable (DataSourceAcq Geometry) Geometry where
    extract1DStreamValue (DataSourceAcq'Geometry'CristalK6C w' mu' komega' kappa' kphi' gamma' delta') i =
        do wavelength <- extract0DStreamValue w'
           mu <- extract0DStreamValue mu'
           komega <- extract0DStreamValue komega'
           kappa <- extract0DStreamValue kappa'
           gamma <- extract0DStreamValue gamma'
           delta <- extract0DStreamValue delta'
           kphi <- extract1DStreamValue kphi' i
           return (Geometry
                   K6c
                   (Source wavelength)
                   (fromList [mu, komega, kappa, kphi, gamma, delta])
                   Nothing)

    extract1DStreamValue (DataSourceAcq'Geometry'Fix w') _i =
        Geometry Fixe <$> extract0DStreamValue w'
                      <*> pure (fromList [])
                      <*> pure Nothing


    extract1DStreamValue (DataSourceAcq'Geometry'Mars w' as') i =
        Geometry Mars <$> extract0DStreamValue w'
                      <*> (fromList <$> do
                             vs <- Prelude.mapM (`extract1DStreamValue` i) as'
                             return (0.0 : vs)) -- TODO check
                      <*> pure Nothing

    extract1DStreamValue (DataSourceAcq'Geometry'MedH w' as') i =
        Geometry MedH <$> extract0DStreamValue w'
                      <*> extract1DStreamValue as' i
                      <*> pure Nothing

    extract1DStreamValue (DataSourceAcq'Geometry'MedV w' as') i =
        Geometry MedV <$> extract0DStreamValue w'
                      <*> extract1DStreamValue as' i
                      <*> pure Nothing

    extract1DStreamValue (DataSourceAcq'Geometry'MedVEiger _w' _as' _eix' _eyz') _i = undefined

    extract1DStreamValue (DataSourceAcq'Geometry'Uhv w' as') i =
        Geometry Uhv <$> extract0DStreamValue w'
                     <*> extract1DStreamValue as' i
                     <*> pure Nothing

    extract1DStreamValue (DataSourceAcq'Geometry'UhvTest w' as') i =
        Geometry Uhv <$> extract0DStreamValue w' -- (Source (unAngstrom w))
                     <*> extract1DStreamValue as' i
                     <*> pure Nothing

instance Is1DStreamable (DataSourceAcq NanoMeter) NanoMeter where
  extract1DStreamValue (DataSourceAcq'NanoMeter d) i = NanoMeter <$> do
    v <- extract1DStreamValue d i
    return $ v *~ angstrom

instance Is1DStreamable Dataset WaveLength where
  extract1DStreamValue d i = do
    v <- extract1DStreamValue d i
    return $ v *~ angstrom

instance Is1DStreamable Dataset Source where
  extract1DStreamValue d i = Source <$> extract1DStreamValue d i

instance Is1DStreamable  [DataSourceAcq Degree] (Data.Vector.Storable.Vector Double) where
  extract1DStreamValue ds i = fromList <$> Prelude.mapM (`extract1DStreamValue` i) ds

----------------
-- DataSource --
----------------

data family DataSourcePath a :: *
data family DataSourceAcq a :: *

class DataSource a where
  withDataSourceP :: (Location l, MonadSafe m) => l -> DataSourcePath a -> (DataSourceAcq a -> m r) -> m r

-- DataSource (instances)

-- Attenuation

data HklBinocularsException
    = WrongAttenuation Text Int Double
    deriving (Show)
instance Exception HklBinocularsException

data instance DataSourcePath Attenuation =
  DataSourcePath'Attenuation { attenuationPath        :: DataSourcePath Float
                             , attenuationOffset      :: Int
                             , attenuationCoefficient :: Double
                             , attenuationMax         :: Maybe Float
                             }
  | DataSourcePath'ApplyedAttenuationFactor { attenuationPath :: DataSourcePath Float }
  | DataSourcePath'NoAttenuation
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq Attenuation =
  DataSourceAcq'Attenuation { attenuationPath        :: DataSourceAcq Float
                            , attenuationOffset      :: Int
                            , attenuationCoefficient :: Double
                            , attenuationMax         :: Maybe Float
                            }
  | DataSourceAcq'ApplyedAttenuationFactor { attenuationPath :: DataSourceAcq Float }
  | DataSourceAcq'NoAttenuation

instance DataSource Attenuation where
  withDataSourceP f (DataSourcePath'Attenuation p o c m) g = withDataSourceP f p $ \ds -> g (DataSourceAcq'Attenuation ds o c m)
  withDataSourceP f (DataSourcePath'ApplyedAttenuationFactor p) g = withDataSourceP f p $ \ds -> g (DataSourceAcq'ApplyedAttenuationFactor ds)
  withDataSourceP _ DataSourcePath'NoAttenuation g = g DataSourceAcq'NoAttenuation

withAttenuationPathP :: (MonadSafe m, Location l) =>
                       l
                     -> DataSourcePath Attenuation
                     -> ((Int -> IO Attenuation) -> m r)
                     -> m r
withAttenuationPathP f p g = withDataSourceP f p $ \a -> g (\j-> extract1DStreamValue a j)


-- Degree

data instance DataSourcePath Degree = DataSourcePath'Degree (Hdf5Path DIM1 Double)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq Degree = DataSourceAcq'Degree Dataset

instance DataSource Degree where
  withDataSourceP f (DataSourcePath'Degree p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Degree ds)

-- Float

data instance DataSourcePath Float = DataSourcePath'Float (Hdf5Path DIM1 Float)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq Float = DataSourceAcq'Float Dataset

instance DataSource Float where
  withDataSourceP f (DataSourcePath'Float p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Float ds)

-- Geometry

data instance DataSourcePath Geometry =
  DataSourcePath'Geometry'CristalK6C { geometryPathWavelength :: DataSourcePath WaveLength
                                     , geometryPathMu         :: DataSourcePath Degree
                                     , geometryPathKomega     :: DataSourcePath Degree
                                     , geometryPathKappa      :: DataSourcePath Degree
                                     , geometryPathKphi       :: DataSourcePath Degree
                                     , geometryPathGamma      :: DataSourcePath Degree
                                     , geometryPathDelta      :: DataSourcePath Degree
                                     }
  | DataSourcePath'Geometry'Fix { geometryPathWavelength :: DataSourcePath WaveLength }
  | DataSourcePath'Geometry'Mars { geometryPathWavelength :: DataSourcePath WaveLength
                                 , geometryPathAxes       :: [DataSourcePath Degree] }
  | DataSourcePath'Geometry'MedH { geometryPathWavelength :: DataSourcePath WaveLength
                                 , geometryPathAxes       :: [DataSourcePath Degree]
                                 }
  | DataSourcePath'Geometry'MedV { geometryPathWavelength :: DataSourcePath WaveLength
                                 , geometryPathBeta       :: DataSourcePath Degree
                                 , geometryPathMu         :: DataSourcePath Degree
                                 , geometryPathOmega      :: DataSourcePath Degree
                                 , geometryPathGamma      :: DataSourcePath Degree
                                 , geometryPathDelta      :: DataSourcePath Degree
                                 , geometryPathEtaa       :: DataSourcePath Degree
                                 }
  | DataSourcePath'Geometry'MedVEiger { geometryPathWavelength :: DataSourcePath WaveLength
                                      , geometryPathAxes       :: [DataSourcePath Degree]
                                      , geometryPathEix        :: DataSourcePath Degree
                                      , geometryPathEiz        :: DataSourcePath Degree
                                      }
  | DataSourcePath'Geometry'Uhv { geometryPathWavelength :: DataSourcePath WaveLength
                                , geometryPathAxes       :: [DataSourcePath Degree]
                                }
  | DataSourcePath'Geometry'UhvTest { geometryPathWavelengthTest :: DataSourcePath WaveLength
                                    , geometryPathAxes           :: [DataSourcePath Degree]
                                    }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq Geometry = DataSourceAcq'Geometry'CristalK6C
                                       (DataSourceAcq WaveLength)
                                       (DataSourceAcq Degree)
                                       (DataSourceAcq Degree)
                                       (DataSourceAcq Degree)
                                       (DataSourceAcq Degree)
                                       (DataSourceAcq Degree)
                                       (DataSourceAcq Degree)
                                     | DataSourceAcq'Geometry'Fix
                                       (DataSourceAcq WaveLength)
                                     | DataSourceAcq'Geometry'Mars
                                       (DataSourceAcq WaveLength)
                                       [DataSourceAcq Degree]
                                     | DataSourceAcq'Geometry'MedH
                                       (DataSourceAcq WaveLength)
                                       [DataSourceAcq Degree]
                                     | DataSourceAcq'Geometry'MedV
                                       (DataSourceAcq WaveLength)
                                       [DataSourceAcq Degree]
                                     | DataSourceAcq'Geometry'MedVEiger
                                       (DataSourceAcq WaveLength)
                                       [DataSourceAcq Degree]
                                       (DataSourceAcq Degree)
                                       (DataSourceAcq Degree)
                                     | DataSourceAcq'Geometry'Uhv
                                       (DataSourceAcq WaveLength)
                                       [DataSourceAcq Degree]
                                     | DataSourceAcq'Geometry'UhvTest
                                       (DataSourceAcq WaveLength)
                                       [DataSourceAcq Degree]

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

withAxesPathP :: (MonadSafe m, Location l) => l -> [DataSourcePath Degree] -> ([DataSourceAcq Degree] -> m a) -> m a
withAxesPathP f dpaths = nest (Prelude.map (withDataSourceP f) dpaths)

instance DataSource Geometry where
  withDataSourceP f (DataSourcePath'Geometry'CristalK6C w m ko ka kp g d) gg =
    withDataSourceP f w $ \w' ->
    withDataSourceP f m $ \mu' ->
    withDataSourceP f ko $ \komega' ->
    withDataSourceP f ka $ \kappa' ->
    withDataSourceP f kp $ \kphi' ->
    withDataSourceP f g $ \gamma' ->
    withDataSourceP f d $ \delta' -> gg (DataSourceAcq'Geometry'CristalK6C w' mu' komega' kappa' kphi' gamma' delta')
  withDataSourceP f (DataSourcePath'Geometry'Fix w) gg =
    withDataSourceP f w $ \w' -> gg (DataSourceAcq'Geometry'Fix w')
  withDataSourceP f (DataSourcePath'Geometry'Mars w as) gg =
    withDataSourceP f w $ \w' ->
    withAxesPathP f as $ \as' -> gg (DataSourceAcq'Geometry'Mars w' as')
  withDataSourceP f (DataSourcePath'Geometry'MedH w as) gg =
    withDataSourceP f w $ \w' ->
    withAxesPathP f as $ \as' -> gg (DataSourceAcq'Geometry'MedH w' as')
  withDataSourceP f (DataSourcePath'Geometry'MedV w b m o g d e) gg =
    withDataSourceP f w $ \w' ->
    withAxesPathP f [b, m, o, g, d, e] $ \as' -> gg (DataSourceAcq'Geometry'MedV w' as')
  withDataSourceP _f (DataSourcePath'Geometry'MedVEiger _w _as _eix _eiz) _gg = undefined
  withDataSourceP f (DataSourcePath'Geometry'Uhv w as) gg =
    withDataSourceP f w $ \w' ->
    withAxesPathP f as $ \as' -> gg (DataSourceAcq'Geometry'Uhv w' as')
  withDataSourceP f (DataSourcePath'Geometry'UhvTest w as) gg =
    withDataSourceP f w $ \w' ->
    withAxesPathP f as $ \as' -> gg (DataSourceAcq'Geometry'UhvTest w' as')

withGeometryPathP :: (MonadSafe m, Location l) => l -> DataSourcePath Geometry -> ((Int -> IO Geometry) -> m r) -> m r
withGeometryPathP f p g = withDataSourceP f p $ \a -> g (\j-> extract1DStreamValue a j)

-- Image

data instance DataSourcePath Image = DataSourcePath'Image (Hdf5Path DIM3 Int32) -- TODO Int32 is wrong
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq Image = DataSourceAcq'Image Dataset

instance DataSource Image where
  withDataSourceP f (DataSourcePath'Image p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Image ds)

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM []          = undefined
condM ((p, v):ls) = ifM p v (condM ls)

withDetectorPathP :: (MonadSafe m, Location l) => l -> Detector a DIM2 -> DataSourcePath Image -> ((Int -> IO Image) -> m r) -> m r
withDetectorPathP f det (DataSourcePath'Image p) g = do
  withHdf5PathP f p $ \p' -> do
    t <- liftIO $ getDatasetType p'
    s <- liftIO $ getTypeSize t
    let n = (size . shape $ det) * fromEnum s
    condM [ ((liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Int32))), (withBytes n $ \buf -> g (\i -> ImageInt32 <$> getArrayInBuffer buf det p' i)))
          , ((liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word16))), (withBytes n $ \buf -> g (\i -> ImageWord16 <$> getArrayInBuffer buf det p' i)))
          , ((liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word32))), (withBytes n $ \buf -> g (\i -> ImageWord32 <$> getArrayInBuffer buf det p' i)))
          ]

-- NanoMeter

data instance DataSourcePath NanoMeter = DataSourcePath'NanoMeter (Hdf5Path Z Double)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq NanoMeter = DataSourceAcq'NanoMeter Dataset

instance DataSource NanoMeter where
  withDataSourceP f (DataSourcePath'NanoMeter p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'NanoMeter ds)

-- WaveLength

data instance DataSourcePath WaveLength = DataSourcePath'WaveLength (Hdf5Path Z Double)
                                        | DataSourcePath'WaveLength'Const Angstrom
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq WaveLength = DataSourceAcq'WaveLength Dataset
                                       | DataSourceAcq'WaveLength'Const Angstrom

instance DataSource WaveLength where
    withDataSourceP f (DataSourcePath'WaveLength p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'WaveLength ds)
    withDataSourceP _ (DataSourcePath'WaveLength'Const a) g = g (DataSourceAcq'WaveLength'Const a)
