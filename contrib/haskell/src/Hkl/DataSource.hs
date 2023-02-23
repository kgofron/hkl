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
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.DataSource
  ( DataSource(..)
  , DataSourcePath(..)
  , DataSourceAcq(..)
  , HklBinocularsException(..)
  , Is0DStreamable(..)
  , Is1DStreamable(..)
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
import           Data.Kind                         (Type)
import           Data.Text                         (Text)
import           Data.Vector.Storable              (Vector, fromList)
import           Data.Vector.Storable.Mutable      (IOVector, unsafeNew)
import           Data.Word                         (Word16, Word32)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (ForeignPtr)
import           GHC.Base                          (returnIO)
import           GHC.Float                         (float2Double)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (degree, (*~), (/~))
import           Pipes.Safe                        (MonadSafe)
import           Test.QuickCheck                   (Arbitrary (..), oneof)

import           Prelude                           hiding (filter)

import           Hkl.Binoculars.Config
import           Hkl.C.Hkl
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Types

--------------------
-- Is0DStreamable --
--------------------

class Is0DStreamable a e where
  extract0DStreamValue :: a -> IO e

instance Is0DStreamable Dataset CDouble where
  extract0DStreamValue d = getPosition d 0

instance Is0DStreamable Dataset Double where
  extract0DStreamValue d = getPosition d 0

instance Is0DStreamable Degree Double where
  extract0DStreamValue (Degree d) = pure (d /~ degree)

instance Is0DStreamable Degree CDouble where
  extract0DStreamValue (Degree d) = pure $ CDouble (d /~ degree)

instance Is0DStreamable (DataSourceAcq Degree) Degree where
    extract0DStreamValue (DataSourceAcq'Degree d) =
        Degree <$> do
          v <- extract0DStreamValue d
          return $ v *~ degree
    extract0DStreamValue (DataSourceAcq'Degree'Const d) = pure d

instance Is0DStreamable (DataSourceAcq Degree) Double where
  extract0DStreamValue (DataSourceAcq'Degree d)       = extract0DStreamValue d
  extract0DStreamValue (DataSourceAcq'Degree'Const d) = extract0DStreamValue d

instance Is0DStreamable (DataSourceAcq Degree) CDouble where
  extract0DStreamValue (DataSourceAcq'Degree d)       = extract0DStreamValue d
  extract0DStreamValue (DataSourceAcq'Degree'Const d) = extract0DStreamValue d

instance Is0DStreamable (DataSourceAcq Double) Double where
  extract0DStreamValue (DataSourceAcq'Double d)       = extract0DStreamValue d
  extract0DStreamValue (DataSourceAcq'Double'Const a) = pure a

--------------------
-- Is1DStreamable --
--------------------

class Is1DStreamable a e where
  extract1DStreamValue :: a -> Int -> IO e

-- Is1DStreamable (instances)

data HklBinocularsException
    = WrongAttenuation Text Int Double
    deriving (Show)
instance Exception HklBinocularsException

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


instance Is1DStreamable (DataSourceAcq Degree) CDouble where
  extract1DStreamValue (DataSourceAcq'Degree d)       = extract1DStreamValue d
  extract1DStreamValue (DataSourceAcq'Degree'Const d) = const $ extract0DStreamValue d

instance Is1DStreamable Dataset CDouble where
  extract1DStreamValue = getPosition

instance Is1DStreamable Dataset Double where
  extract1DStreamValue = getPosition

instance Is1DStreamable Dataset Float where
  extract1DStreamValue = getPosition

instance Is1DStreamable (DataSourceAcq Float) Float where
  extract1DStreamValue (DataSourceAcq'Float ds) = extract1DStreamValue ds

instance Is1DStreamable (DataSourceAcq Float) Attenuation where
  extract1DStreamValue (DataSourceAcq'Float ds) i = Attenuation <$> extract1DStreamValue ds i

instance Is1DStreamable (DataSourceAcq Geometry) (ForeignPtr C'HklGeometry) where
    extract1DStreamValue (DataSourceAcq'Geometry'MedVEiger _w' _as' _eix' _eyz') _i = undefined
    extract1DStreamValue (DataSourceAcq'Geometry fptr w' as') i = do
      w <- extract0DStreamValue w'
      as <- extract1DStreamValue as' i
      pokeGeometry fptr w as
      return fptr

instance Is1DStreamable (DataSourceAcq Image) Image where
  extract1DStreamValue (DataSourceAcq'Image'Int32 ds det buf) i = ImageInt32 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Word16 ds det buf) i = ImageWord16 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Word32 ds det buf) i = ImageWord32 <$> getArrayInBuffer buf det ds i

instance Is1DStreamable (DataSourceAcq Index) Index where
  extract1DStreamValue (DataSourceAcq'Index ds) i = Index <$> extract1DStreamValue ds i
  extract1DStreamValue DataSourceAcq'Index'NoIndex _ = returnIO $ Index 0

instance Is1DStreamable  [DataSourceAcq Degree] (Data.Vector.Storable.Vector CDouble) where
  extract1DStreamValue ds i = fromList <$> Prelude.mapM (`extract1DStreamValue` i) ds

----------------
-- DataSource --
----------------

class DataSource a where
  data DataSourcePath a :: Type
  data DataSourceAcq a :: Type

  withDataSourceP :: (Location l, MonadSafe m) => l -> DataSourcePath a -> (DataSourceAcq a -> m r) -> m r

-- DataSource (instances)

-- Attenuation

instance DataSource Attenuation where
  data DataSourcePath Attenuation =
    DataSourcePath'Attenuation { attenuationPath            :: DataSourcePath Float
                               , attenuationPathOffset      :: Int
                               , attenuationPathCoefficient :: Double
                               , attenuationPathMax         :: Maybe Float
                               }
    | DataSourcePath'ApplyedAttenuationFactor { attenuationPath :: DataSourcePath Float }
    | DataSourcePath'NoAttenuation
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Attenuation =
    DataSourceAcq'Attenuation { attenuationAcqPath        :: DataSourceAcq Float
                              , attenuationAcqOffset      :: Int
                              , attenuationAcqCoefficient :: Double
                              , attenuationAcqMax         :: Maybe Float
                              }
    | DataSourceAcq'ApplyedAttenuationFactor { attenuationAcqPath :: DataSourceAcq Float }
    | DataSourceAcq'NoAttenuation

  withDataSourceP f (DataSourcePath'Attenuation p o c m) g = withDataSourceP f p $ \ds -> g (DataSourceAcq'Attenuation ds o c m)
  withDataSourceP f (DataSourcePath'ApplyedAttenuationFactor p) g = withDataSourceP f p $ \ds -> g (DataSourceAcq'ApplyedAttenuationFactor ds)
  withDataSourceP _ DataSourcePath'NoAttenuation g = g DataSourceAcq'NoAttenuation

instance Arbitrary (DataSourcePath Attenuation) where
  arbitrary = oneof
    [ DataSourcePath'Attenuation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DataSourcePath'ApplyedAttenuationFactor <$> arbitrary
    , pure DataSourcePath'NoAttenuation
    ]


-- Degree

instance DataSource Degree where
  data DataSourcePath Degree
    = DataSourcePath'Degree (Hdf5Path DIM1 Double)
    | DataSourcePath'Degree'Const Degree
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Degree
    = DataSourceAcq'Degree Dataset
    | DataSourceAcq'Degree'Const Degree

  withDataSourceP f (DataSourcePath'Degree p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Degree ds)
  withDataSourceP _ (DataSourcePath'Degree'Const d) g = g (DataSourceAcq'Degree'Const d)

instance Arbitrary (DataSourcePath Degree) where
  arbitrary = oneof
    [ DataSourcePath'Degree <$> arbitrary
    , DataSourcePath'Degree'Const <$> arbitrary
    ]

-- Double

instance DataSource Double where
  data DataSourcePath Double
    = DataSourcePath'Double (Hdf5Path Z Double)
    | DataSourcePath'Double'Const Double
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Double
    = DataSourceAcq'Double Dataset
    | DataSourceAcq'Double'Const Double

  withDataSourceP f (DataSourcePath'Double p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Double ds)
  withDataSourceP _ (DataSourcePath'Double'Const a) g = g (DataSourceAcq'Double'Const a)

instance Arbitrary (DataSourcePath Double) where
  arbitrary = oneof
    [ DataSourcePath'Double <$> arbitrary
    , DataSourcePath'Double'Const <$> arbitrary
    ]


-- Float

instance DataSource Float where
  newtype DataSourcePath Float
    = DataSourcePath'Float (Hdf5Path DIM1 Float)
    deriving (Generic, Show, FromJSON, ToJSON)

  newtype DataSourceAcq Float
    = DataSourceAcq'Float Dataset

  withDataSourceP f (DataSourcePath'Float p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Float ds)

instance Arbitrary (DataSourcePath Float) where
  arbitrary = DataSourcePath'Float <$> arbitrary


-- Geometry

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

withAxesPathP :: (MonadSafe m, Location l) => l -> [DataSourcePath Degree] -> ([DataSourceAcq Degree] -> m a) -> m a
withAxesPathP f dpaths = nest (Prelude.map (withDataSourceP f) dpaths)

instance DataSource Geometry where
  data DataSourcePath Geometry =
    DataSourcePath'Geometry { geometryGeometry       :: Geometry
                            , geometryPathWavelength :: DataSourcePath Double
                            , geometryPathAxes       :: [DataSourcePath Degree]
                            }
    | DataSourcePath'Geometry'Fix { geometryPathWavelength :: DataSourcePath Double }
    | DataSourcePath'Geometry'MedVEiger { geometryPathWavelength :: DataSourcePath Double
                                        , geometryPathAxes       :: [DataSourcePath Degree]
                                        , geometryPathEix        :: DataSourcePath Degree
                                        , geometryPathEiz        :: DataSourcePath Degree
                                        }
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Geometry
    = DataSourceAcq'Geometry'MedVEiger
      (DataSourceAcq Double)
      [DataSourceAcq Degree]
      (DataSourceAcq Degree)
      (DataSourceAcq Degree)
    | DataSourceAcq'Geometry
      (ForeignPtr C'HklGeometry)
      (DataSourceAcq Double)
      [DataSourceAcq Degree]


  withDataSourceP f (DataSourcePath'Geometry g w as) gg =
    withDataSourceP f w $ \w' ->
    withAxesPathP f as $ \as' -> do
    fptr <- liftIO $ newGeometry g
    gg (DataSourceAcq'Geometry fptr w' as')
  withDataSourceP f (DataSourcePath'Geometry'Fix w) gg =
    withDataSourceP f w $ \w' -> do
    fptr <- liftIO $ newGeometry fixed
    gg (DataSourceAcq'Geometry fptr w' [])
  withDataSourceP _f (DataSourcePath'Geometry'MedVEiger _w _as _eix _eiz) _gg = undefined

instance Arbitrary (DataSourcePath Geometry) where
  arbitrary = oneof
    -- TODO add the Geometry constructor
    [ DataSourcePath'Geometry'Fix <$> arbitrary
    , DataSourcePath'Geometry'MedVEiger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

-- Image

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM []          = undefined
condM ((p, v):ls) = ifM p v (condM ls)

instance DataSource Image where
  data DataSourcePath Image
    = DataSourcePath'Image (Hdf5Path DIM3 Int32) (Detector Hkl DIM2) -- TODO Int32 is wrong
    deriving (Generic, Show, FromJSON, ToJSON)

  data instance DataSourceAcq Image = DataSourceAcq'Image'Int32 Dataset (Detector Hkl DIM2) (IOVector Int32)
                                    | DataSourceAcq'Image'Word16 Dataset (Detector Hkl DIM2) (IOVector Word16)
                                    | DataSourceAcq'Image'Word32 Dataset (Detector Hkl DIM2) (IOVector Word32)

  withDataSourceP f (DataSourcePath'Image p det) g = withHdf5PathP f p $ \ds -> do
    t <- liftIO $ getDatasetType ds
    s <- liftIO $ getTypeSize t
    let n = (size . shape $ det) * fromEnum s
    condM [ (liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Int32)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Int32 ds det arr))
          , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word16)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Word16 ds det arr))
          , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word32)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Word32 ds det arr))
          ]

instance Arbitrary (DataSourcePath Image) where
  arbitrary = DataSourcePath'Image <$> arbitrary <*> arbitrary

-- Index

instance DataSource Index where
  data DataSourcePath Index
    = DataSourcePath'Index (Hdf5Path DIM1 Double)
    | DataSourcePath'Index'NoIndex
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Index = DataSourceAcq'Index Dataset
                           | DataSourceAcq'Index'NoIndex

  withDataSourceP f (DataSourcePath'Index p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Index ds)
  withDataSourceP _ DataSourcePath'Index'NoIndex g = g DataSourceAcq'Index'NoIndex

instance Arbitrary (DataSourcePath Index) where
  arbitrary = DataSourcePath'Index <$> arbitrary

-- Int

instance DataSource Int where
  newtype DataSourcePath Int
    = DataSourcePath'Int Int
    deriving (Generic, Show, FromJSON, ToJSON)

  newtype DataSourceAcq Int
    = DataSourceAcq'Int Int

  withDataSourceP _ (DataSourcePath'Int p) g = g (DataSourceAcq'Int p)
