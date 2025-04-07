{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
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
    Copyright  : Copyright (C) 2014-2025 Synchrotron SOLEIL
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
  , DataSourceShape(..)
  , HklBinocularsException(..)
  , Is0DStreamable(..)
  , Is1DStreamable(..)
  , combine'Shape
  , length'DataSourceShape
  ) where

import           Bindings.HDF5.Core                (Location)
import           Bindings.HDF5.Dataset             (getDatasetSpace,
                                                    getDatasetType)
import           Bindings.HDF5.Dataspace           (getSimpleDataspaceExtentNPoints)
import           Bindings.HDF5.Datatype            (getTypeSize, nativeTypeOf,
                                                    typeIDsEqual)
import           Control.Exception                 (throwIO)
import           Control.Monad.Extra               (ifM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Data.Int                          (Int32)
import           Data.Kind                         (Type)
import           Data.Text                         (Text, unpack)
import           Data.Vector.Storable              (Vector, fromList)
import           Data.Vector.Storable.Mutable      (IOVector, replicate,
                                                    unsafeNew)
import           Data.Word                         (Word16, Word32)
import           Foreign.C.Types                   (CDouble (..))
import           GHC.Base                          (returnIO)
import           GHC.Float                         (float2Double)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (degree, (*~), (/~))
import           Pipes.Safe                        (MonadSafe, catch, throwM)
import           Text.Printf                       (printf)

import           Prelude                           hiding (filter, replicate)

import           Hkl.Binoculars.Config
import           Hkl.Detector
import           Hkl.Exception
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Repa
import           Hkl.Types

--------------------
-- Is0DStreamable --
--------------------

class Is0DStreamable a e where
  extract0DStreamValue :: a -> IO e

-- Is0DStreamable (instances)

instance Is0DStreamable Dataset CDouble where
  extract0DStreamValue d = getPosition d 0

instance Is0DStreamable Dataset Double where
  extract0DStreamValue d = getPosition d 0

instance Is0DStreamable Degree Double where
  extract0DStreamValue (Degree d) = pure (d /~ degree)

instance Is0DStreamable Degree CDouble where
  extract0DStreamValue (Degree d) = pure $ CDouble (d /~ degree)

instance Is0DStreamable Double CDouble where
  extract0DStreamValue d = pure $ CDouble d

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

instance Is0DStreamable (DataSourceAcq Scannumber) Scannumber where
  extract0DStreamValue (DataSourceAcq'Scannumber'Const sn) = pure sn

instance Is0DStreamable (DataSourceAcq Timescan0) Timescan0 where
  extract0DStreamValue (DataSourceAcq'Timescan0 ds) = Timescan0 <$> extract0DStreamValue ds
  extract0DStreamValue DataSourceAcq'Timescan0'NoTimescan0 = returnIO $ Timescan0 0

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


instance Is1DStreamable (DataSourceAcq Double) CDouble where
  extract1DStreamValue (DataSourceAcq'Double d)       = extract1DStreamValue d
  extract1DStreamValue (DataSourceAcq'Double'Const d) = const $ extract0DStreamValue d

instance Is1DStreamable Dataset CDouble where
  extract1DStreamValue = getPosition

instance Is1DStreamable Dataset Double where
  extract1DStreamValue = getPosition

instance Is1DStreamable Dataset Float where
  extract1DStreamValue = getPosition

instance Is1DStreamable (DataSourceAcq [Double]) (Data.Vector.Storable.Vector CDouble) where
  extract1DStreamValue (DataSourceAcq'List ds) i = fromList <$> Prelude.mapM (`extract1DStreamValue` i) ds

instance Is1DStreamable (DataSourceAcq Float) Float where
  extract1DStreamValue (DataSourceAcq'Float ds) = extract1DStreamValue ds

instance Is1DStreamable (DataSourceAcq Float) Attenuation where
  extract1DStreamValue (DataSourceAcq'Float ds) i = Attenuation <$> extract1DStreamValue ds i

instance  Is1DStreamable (DataSourceAcq Geometry) Geometry where
     extract1DStreamValue (DataSourceAcq'Geometry g w' as') i =
         do w <- extract0DStreamValue w'
            as <- extract1DStreamValue as' i
            let state = GeometryState w as
            pure $ case g of
                     (Geometry'Custom axes _) -> Geometry'Custom axes (Just state)
                     (Geometry'Factory factory _) -> Geometry'Factory factory (Just state)

instance Is1DStreamable (DataSourceAcq Image) Image where
  extract1DStreamValue (DataSourceAcq'Image'Dummy buf) _ = pure $ ImageDouble buf
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Double det ds buf) i = ImageDouble <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Int32 det ds buf) i = ImageInt32 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Word16 det ds buf) i = ImageWord16 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Word32 det ds buf) i = ImageWord32 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Img'Int32 det buf _ tmpl sn fn) i = ImageInt32 <$> readImgInBuffer buf det (fn tmpl sn i)

instance Is1DStreamable (DataSourceAcq Mask) (Maybe Mask) where
    extract1DStreamValue (DataSourceAcq'Mask'NoMask) _ = returnIO Nothing
    extract1DStreamValue (DataSourceAcq'Mask m) _      = returnIO (Just m)

instance Is1DStreamable (DataSourceAcq Timestamp) Timestamp where
  extract1DStreamValue (DataSourceAcq'Timestamp ds) i = Timestamp <$> extract1DStreamValue ds i
  extract1DStreamValue DataSourceAcq'Timestamp'NoTimestamp _ = returnIO $ Timestamp 0

----------------
-- DataSource --
----------------

data DataSourceShape
    = DataSourceShape'Range !DIM1 !DIM1

combine'Shape :: DataSourceShape -> DataSourceShape -> DataSourceShape
combine'Shape (DataSourceShape'Range _ (Z :. 1)) s = s
combine'Shape s (DataSourceShape'Range _ (Z :. 1)) = s
combine'Shape (DataSourceShape'Range (Z :. f1) (Z :. t1)) (DataSourceShape'Range (Z :. f2) (Z :. t2))
    = DataSourceShape'Range (ix1 (max f1 f2)) (ix1 (min t1 t2))


shape1 :: DataSourceShape
shape1 = DataSourceShape'Range (Z :. 0) (Z :. 1)

ds'Shape'Dataset :: Dataset -> IO DataSourceShape
ds'Shape'Dataset ds = do
  (t:_, mt:_) <- datasetShape ds
  case mt of
    Just t' -> pure $ DataSourceShape'Range (ix1 0) (ix1 (fromIntegral t'))
    Nothing -> pure $ DataSourceShape'Range (ix1 0) (ix1 (fromIntegral t))

length'DataSourceShape :: DataSourceShape -> Int
length'DataSourceShape (DataSourceShape'Range (Z :. f) (Z :. t)) = t - f

class DataSource a where
  data DataSourcePath a :: Type
  data DataSourceAcq a :: Type

  ds'Shape :: MonadSafe m => DataSourceAcq a -> m DataSourceShape

  withDataSourceP :: (Location l, MonadSafe m)
                    => ScanFile l -> DataSourcePath a -> (DataSourceAcq a -> m r) -> m r

  withDataSourcePOr :: (Location l, MonadSafe m)
                    => ScanFile l -> DataSourcePath a -> DataSourcePath a -> (DataSourceAcq a -> m r) -> m r
  withDataSourcePOr f l r g = withDataSourceP f l g
                              `catch`
                              \exl -> withDataSourceP f r g
                                     `catch`
                                     \exr -> throwM $ CanNotOpenDataSource'Or exl exr

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
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Attenuation =
    DataSourceAcq'Attenuation { attenuationAcqPath        :: DataSourceAcq Float
                              , attenuationAcqOffset      :: Int
                              , attenuationAcqCoefficient :: Double
                              , attenuationAcqMax         :: Maybe Float
                              }
    | DataSourceAcq'ApplyedAttenuationFactor { attenuationAcqPath :: DataSourceAcq Float }
    | DataSourceAcq'NoAttenuation

  ds'Shape (DataSourceAcq'Attenuation fp _ _ _)        = ds'Shape fp
  ds'Shape (DataSourceAcq'ApplyedAttenuationFactor fp) = ds'Shape fp
  ds'Shape DataSourceAcq'NoAttenuation                 = undefined

  withDataSourceP f (DataSourcePath'Attenuation p o c m) g = withDataSourceP f p $ \ds -> g (DataSourceAcq'Attenuation ds o c m)
  withDataSourceP f (DataSourcePath'ApplyedAttenuationFactor p) g = withDataSourceP f p $ \ds -> g (DataSourceAcq'ApplyedAttenuationFactor ds)
  withDataSourceP _ DataSourcePath'NoAttenuation g = g DataSourceAcq'NoAttenuation

-- Degree

instance DataSource Degree where
  data DataSourcePath Degree
    = DataSourcePath'Degree (Hdf5Path DIM1 Double)
    | DataSourcePath'Degree'Const Degree
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Degree
    = DataSourceAcq'Degree Dataset
    | DataSourceAcq'Degree'Const Degree

  ds'Shape (DataSourceAcq'Degree ds)      = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Degree'Const _) = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Degree p) g
    = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Degree ds)
  withDataSourceP _ (DataSourcePath'Degree'Const d) g = g (DataSourceAcq'Degree'Const d)

-- Double

instance DataSource Double where
  data DataSourcePath Double
    = DataSourcePath'Double (Hdf5Path Z Double)
    | DataSourcePath'Double'Ini ConfigContent Section Key
    | DataSourcePath'Double'Const Double
    | DataSourcePath'Double'Or (DataSourcePath Double) (DataSourcePath Double)
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Double
    = DataSourceAcq'Double Dataset
    | DataSourceAcq'Double'Const Double

  ds'Shape (DataSourceAcq'Double ds)      = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Double'Const _) = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Double p) g = withHdf5PathP f p $ \ds -> do
    space <- liftIO $ getDatasetSpace ds
    l <- liftIO $ getSimpleDataspaceExtentNPoints space
    case l of
      1 -> do
        v <- liftIO $ extract0DStreamValue ds
        g (DataSourceAcq'Double'Const v)
      _ -> g (DataSourceAcq'Double ds)
  withDataSourceP _ (DataSourcePath'Double'Const a) g = g (DataSourceAcq'Double'Const a)
  withDataSourceP _ (DataSourcePath'Double'Ini (ConfigContent cfg) s k) g =
      eitherF (const $ throwM $ CanNotOpenDataSource'Double'Ini s k) (parse' cfg s k)
      (\mv -> case mv of
               Nothing -> throwM $ CanNotOpenDataSource'Double'Ini s k
               Just v  ->  g (DataSourceAcq'Double'Const v))
  withDataSourceP f (DataSourcePath'Double'Or l r) g = withDataSourcePOr f l r g

-- [Double]

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

instance DataSource [Double] where
    data DataSourcePath [Double]
        = DataSourcePath'List [DataSourcePath Double]
          deriving (Generic, FromJSON, Show, ToJSON)

    data DataSourceAcq [Double]
        = DataSourceAcq'List [DataSourceAcq Double]

    ds'Shape  (DataSourceAcq'List ds)
        = do ss <- mapM ds'Shape ds
             pure $ foldl1 combine'Shape ss

    withDataSourceP f (DataSourcePath'List ps) g
        = nest (Prelude.map (withDataSourceP f) ps) (\as -> g $ (DataSourceAcq'List as))

-- Float

instance DataSource Float where
  newtype DataSourcePath Float
    = DataSourcePath'Float (Hdf5Path DIM1 Float)
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

  newtype DataSourceAcq Float
    = DataSourceAcq'Float Dataset

  ds'Shape (DataSourceAcq'Float ds) = liftIO $ ds'Shape'Dataset ds

  withDataSourceP (ScanFile f _) (DataSourcePath'Float p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Float ds)

-- Geometry

instance DataSource Geometry where
  data DataSourcePath Geometry =
    DataSourcePath'Geometry { geometryGeometry       :: Geometry
                            , geometryPathWavelength :: DataSourcePath Double
                            , geometryPathAxes       :: DataSourcePath [Double]
                            }
    | DataSourcePath'Geometry'Fix { geometryPathWavelength :: DataSourcePath Double }
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Geometry
    = DataSourceAcq'Geometry
      Geometry
      (DataSourceAcq Double)
      (DataSourceAcq [Double])

  ds'Shape (DataSourceAcq'Geometry _ w as)
      = do sw <- ds'Shape w
           sas <- ds'Shape as
           pure $ sw `combine'Shape` sas

  withDataSourceP f (DataSourcePath'Geometry g w as) gg =
    withDataSourceP f w $ \w' ->
    withDataSourceP f as $ \as' -> do
    gg (DataSourceAcq'Geometry g w' as')
  withDataSourceP f (DataSourcePath'Geometry'Fix w) gg =
    withDataSourceP f w $ \w' -> do
    gg (DataSourceAcq'Geometry fixed w' (DataSourceAcq'List []))

-- Image

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM []          = undefined
condM ((p, v):ls) = ifM p v (condM ls)

instance DataSource Image where
  data instance DataSourcePath Image
    = DataSourcePath'Image'Dummy (Detector Hkl DIM2) Double
    | DataSourcePath'Image'Hdf5 (Detector Hkl DIM2) (Hdf5Path DIM3 Int32) -- TODO Int32 is wrong
    | DataSourcePath'Image'Img (Detector Hkl DIM2) (DataSourcePath Attenuation) Text Scannumber
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

  data instance DataSourceAcq Image
      = DataSourceAcq'Image'Dummy (IOVector Double)
      | DataSourceAcq'Image'Hdf5'Double (Detector Hkl DIM2) Dataset (IOVector Double)
      | DataSourceAcq'Image'Hdf5'Int32 (Detector Hkl DIM2) Dataset (IOVector Int32)
      | DataSourceAcq'Image'Hdf5'Word16 (Detector Hkl DIM2) Dataset (IOVector Word16)
      | DataSourceAcq'Image'Hdf5'Word32 (Detector Hkl DIM2) Dataset (IOVector Word32)
      | DataSourceAcq'Image'Img'Int32 (Detector Hkl DIM2) (IOVector Int32) (DataSourceAcq Attenuation) Text Scannumber (Text -> Scannumber -> Int -> FilePath)

  ds'Shape (DataSourceAcq'Image'Dummy _)             = pure $ DataSourceShape'Range (ix1 0) (ix1 1)
  ds'Shape (DataSourceAcq'Image'Hdf5'Double _ ds _)  = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Hdf5'Int32 _ ds _)   = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Hdf5'Word16 _ ds _)  = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Hdf5'Word32 _ ds _)  = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Img'Int32 _ _ a _ _ _) = ds'Shape a

  withDataSourceP _ (DataSourcePath'Image'Dummy det v) g
      =  do let n = (size . shape $ det)
            arr <- liftIO $ replicate n v
            g (DataSourceAcq'Image'Dummy arr)

  withDataSourceP (ScanFile f _) (DataSourcePath'Image'Hdf5 det p) g = withHdf5PathP f p $ \ds -> do
    t <- liftIO $ getDatasetType ds
    s <- liftIO $ getTypeSize t
    let n = (size . shape $ det) * fromEnum s
    condM [ (liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Double)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Hdf5'Double det ds arr))
          , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Int32)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Hdf5'Int32 det ds arr))
          , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word16)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Hdf5'Word16 det ds arr))
          , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word32)), do
                arr <- liftIO $ unsafeNew n
                g (DataSourceAcq'Image'Hdf5'Word32 det ds arr))
          ]

  withDataSourceP f@(ScanFile _ sn) (DataSourcePath'Image'Img det a tmpl (Scannumber sn0)) g
    = withDataSourceP f a $ \a' -> do
    let n = (size . shape $ det)
    arr <- liftIO $ unsafeNew n
    g (DataSourceAcq'Image'Img'Int32 det arr a' tmpl sn f')
      where
        f' :: Text -> Scannumber -> Int -> FilePath
        f' tmpl' (Scannumber sn') i = printf (unpack tmpl') sn0 sn0 ((sn' - sn0) * 1029 + i)

instance HasFieldValue (DataSourcePath Image) where
    fieldvalue = autoJSON

-- Int

instance DataSource Int where
  newtype DataSourcePath Int
    = DataSourcePath'Int Int
    deriving (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

  ds'Shape _ = pure $ shape1

  newtype DataSourceAcq Int
    = DataSourceAcq'Int Int

  withDataSourceP _ (DataSourcePath'Int p) g = g (DataSourceAcq'Int p)

-- Mask

instance DataSource Mask where
    data DataSourcePath Mask
        = DataSourcePath'Mask'NoMask
        | DataSourcePath'Mask MaskLocation (Detector Hkl DIM2)
          deriving (Generic, Show)
          deriving anyclass (FromJSON, ToJSON)

    data DataSourceAcq Mask
        = DataSourceAcq'Mask Mask
        | DataSourceAcq'Mask'NoMask

    ds'Shape _ = pure $ shape1

    withDataSourceP _ DataSourcePath'Mask'NoMask g = g DataSourceAcq'Mask'NoMask
    withDataSourceP (ScanFile _ sn)  (DataSourcePath'Mask l d) g
        = do  m <- getMask l d sn
              g (DataSourceAcq'Mask m)

-- Scannumber

instance DataSource Scannumber where
  data DataSourcePath Scannumber
    = DataSourcePath'Scannumber
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Scannumber = DataSourceAcq'Scannumber'Const Scannumber

  ds'Shape _ = pure $ shape1

  withDataSourceP (ScanFile _ s) DataSourcePath'Scannumber g = g (DataSourceAcq'Scannumber'Const s)


-- Timestamp

instance DataSource Timestamp where
  data DataSourcePath Timestamp
    = DataSourcePath'Timestamp (Hdf5Path DIM1 Double)
    | DataSourcePath'Timestamp'NoTimestamp
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Timestamp = DataSourceAcq'Timestamp Dataset
                               | DataSourceAcq'Timestamp'NoTimestamp

  ds'Shape (DataSourceAcq'Timestamp ds)        = liftIO $ ds'Shape'Dataset ds
  ds'Shape DataSourceAcq'Timestamp'NoTimestamp = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Timestamp p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Timestamp ds)
  withDataSourceP _ DataSourcePath'Timestamp'NoTimestamp g = g DataSourceAcq'Timestamp'NoTimestamp

-- Timescan0

instance DataSource Timescan0 where
  data DataSourcePath Timescan0
    = DataSourcePath'Timescan0 (Hdf5Path DIM1 Double)
    | DataSourcePath'Timescan0'NoTimescan0
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq Timescan0 = DataSourceAcq'Timescan0 Dataset
                               | DataSourceAcq'Timescan0'NoTimescan0

  ds'Shape (DataSourceAcq'Timescan0 ds)        = liftIO $ ds'Shape'Dataset ds
  ds'Shape DataSourceAcq'Timescan0'NoTimescan0 = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Timescan0 p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Timescan0 ds)
  withDataSourceP _ DataSourcePath'Timescan0'NoTimescan0 g = g DataSourceAcq'Timescan0'NoTimescan0
