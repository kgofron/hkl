{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

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
  , DataSourceShape(..)
  , DSKind(..)
  , DSWrap_
  , DSAttenuation(..)
  , DSDegree(..)
  , DSDouble(..)
  , DSDoubles(..)
  , DSFloat(..)
  , DSGeometry(..)
  , DSImage(..)
  , DSMask(..)
  , DSTimestamp(..)
  , DSTimescan0(..)
  , DSScannumber(..)
  , HklBinocularsException(..)
  , Is0DStreamable(..)
  , Is1DStreamable(..)
  , combine'Shape
  , length'DataSourceShape
  , withDataSourcesP
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
#if !MIN_VERSION_base(4, 18, 0)
import           GHC.Base                          (liftA2)
#endif
import           GHC.Base                          (returnIO)
import           GHC.Float                         (float2Double)
import           GHC.Generics                      (Generic, K1 (..), M1 (..),
                                                    Rep (..), (:*:) (..),
                                                    (:+:) (..))
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

instance Is0DStreamable (DSDegree DSAcq) Degree where
    extract0DStreamValue (DataSourceAcq'Degree d) =
        Degree <$> do
          v <- extract0DStreamValue d
          return $ v *~ degree
    extract0DStreamValue (DataSourceAcq'Degree'Const d) = pure d

instance Is0DStreamable (DSDegree DSAcq) Double where
  extract0DStreamValue (DataSourceAcq'Degree d)       = extract0DStreamValue d
  extract0DStreamValue (DataSourceAcq'Degree'Const d) = extract0DStreamValue d

instance Is0DStreamable (DSDegree DSAcq) CDouble where
  extract0DStreamValue (DataSourceAcq'Degree d)       = extract0DStreamValue d
  extract0DStreamValue (DataSourceAcq'Degree'Const d) = extract0DStreamValue d

instance Is0DStreamable (DSDouble DSAcq) Double where
  extract0DStreamValue (DataSourceAcq'Double d)       = extract0DStreamValue d
  extract0DStreamValue (DataSourceAcq'Double'Const a) = pure a

instance Is0DStreamable (DSScannumber DSAcq) Scannumber where
  extract0DStreamValue (DataSourceAcq'Scannumber'Const sn) = pure sn

instance Is0DStreamable (DSTimescan0 DSAcq) Timescan0 where
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

instance Is1DStreamable (DSAttenuation DSAcq) Attenuation where
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

    extract1DStreamValue (DataSourceAcq'ApplyedAttenuationFactor ds) i = Attenuation . float2Double <$> extract1DStreamValue ds i

    extract1DStreamValue DataSourceAcq'NoAttenuation _ = returnIO $ Attenuation 1


instance Is1DStreamable (DSDouble DSAcq) CDouble where
  extract1DStreamValue (DataSourceAcq'Double d)       = extract1DStreamValue d
  extract1DStreamValue (DataSourceAcq'Double'Const d) = const $ extract0DStreamValue d

instance Is1DStreamable Dataset CDouble where
  extract1DStreamValue = getPosition

instance Is1DStreamable Dataset Double where
  extract1DStreamValue = getPosition

instance Is1DStreamable Dataset Float where
  extract1DStreamValue = getPosition

instance Is1DStreamable (DSDoubles DSAcq) (Data.Vector.Storable.Vector CDouble) where
  extract1DStreamValue (DataSourceAcq'List ds) i = fromList <$> Prelude.mapM (`extract1DStreamValue` i) ds

instance Is1DStreamable (DSFloat DSAcq) Float where
  extract1DStreamValue (DataSourceAcq'Float ds) = extract1DStreamValue ds

instance  Is1DStreamable (DSGeometry DSAcq) Geometry where
     extract1DStreamValue (DataSourceAcq'Geometry g w' as') i =
         do w <- extract0DStreamValue w'
            as <- extract1DStreamValue as' i
            let state = GeometryState w as
            pure $ case g of
                     (Geometry'Custom axes _) -> Geometry'Custom axes (Just state)
                     (Geometry'Factory factory _) -> Geometry'Factory factory (Just state)

instance Is1DStreamable (DSImage DSAcq) Image where
  extract1DStreamValue (DataSourceAcq'Image'Dummy buf) _ = pure $ ImageDouble buf
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Double det ds buf) i = ImageDouble <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Int32 det ds buf) i = ImageInt32 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Word16 det ds buf) i = ImageWord16 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Hdf5'Word32 det ds buf) i = ImageWord32 <$> getArrayInBuffer buf det ds i
  extract1DStreamValue (DataSourceAcq'Image'Img'Int32 det buf tmpl sn fn) i = ImageInt32 <$> readImgInBuffer buf det (fn tmpl sn i)

instance Is1DStreamable (DSMask DSAcq) (Maybe Mask) where
    extract1DStreamValue (DataSourceAcq'Mask'NoMask) _ = returnIO Nothing
    extract1DStreamValue (DataSourceAcq'Mask m) _      = returnIO (Just m)

instance Is1DStreamable (DSTimestamp DSAcq) Timestamp where
  extract1DStreamValue (DataSourceAcq'Timestamp ds) i = Timestamp <$> extract1DStreamValue ds i
  extract1DStreamValue DataSourceAcq'Timestamp'NoTimestamp _ = returnIO $ Timestamp 0

----------------
-- DataSource --
----------------

data DSKind = DSPath | DSAcq

type family DSWrap (k :: DSKind) (t :: Type) :: Type where
   DSWrap DSAcq t = t
   DSWrap DSPath t = [t]

type DSWrap_ f k = DSWrap k (f k)

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

-- | Generic 'ds'Shape'

generic'ds'Shape :: ( MonadSafe m
                   , Generic (d DSAcq)
                   , GDataSourceAcq (Rep (d DSAcq))
                   )
                 => d DSAcq -> m DataSourceShape
generic'ds'Shape = g'ds'Shape . from

class GDataSourceAcq dataAcq where
   g'ds'Shape :: MonadSafe m => dataAcq x -> m DataSourceShape

instance GDataSourceAcq f => GDataSourceAcq (M1 i c f) where
   g'ds'Shape (M1 f) = g'ds'Shape f

instance (GDataSourceAcq f, GDataSourceAcq f') => GDataSourceAcq (f :*: f') where
   g'ds'Shape (f :*: f') = liftA2 combine'Shape (g'ds'Shape f) (g'ds'Shape f')

instance (GDataSourceAcq f, GDataSourceAcq f') => GDataSourceAcq (f :+: f') where
   g'ds'Shape (L1 f)  = g'ds'Shape f
   g'ds'Shape (R1 f') = g'ds'Shape f'

instance DataSource a => GDataSourceAcq (K1 i (a DSAcq)) where
   g'ds'Shape (K1 acq) = ds'Shape acq

-- | Generic 'withDataSourceP'

generic'withDataSourceP :: ( Generic (d DSPath)
                          , Generic (d DSAcq)
                          , GDataSourcePath (Rep (d DSPath)) (Rep (d DSAcq))
                          , Location l
                          , MonadSafe m
                          )
                        => ScanFile l -> d DSPath -> (d DSAcq -> m r) -> m r
generic'withDataSourceP file src gg = g'withDataSourceP file (from src) (gg . to)


class GDataSourcePath dataPath dataAcq where
    g'withDataSourceP :: (Location l, MonadSafe m)
                      => ScanFile l -> dataPath x -> (dataAcq x -> m r) -> m r

instance GDataSourcePath f g => GDataSourcePath (M1 i c f) (M1 i c' g) where
    g'withDataSourceP f (M1 d) gg = g'withDataSourceP f d (gg . M1)

instance (GDataSourcePath f g, GDataSourcePath f' g') => GDataSourcePath (f :*: f') (g :*: g') where
    g'withDataSourceP file (f :*: f') gg =
        g'withDataSourceP file f $ \g ->
        g'withDataSourceP file f' $ \g' ->
            gg (g :*: g')

instance (Show (a DSPath), DataSource a) => GDataSourcePath (K1 i [a DSPath]) (K1 i (a DSAcq)) where
    g'withDataSourceP file (K1 acq) gg =
        withDataSourcesP file acq $ \dat ->
            gg (K1 dat)

-- DataSource

class DataSource d where
    ds'Shape :: MonadSafe m => d DSAcq -> m DataSourceShape
    withDataSourceP :: (Location l, MonadSafe m) => ScanFile l -> d DSPath -> (d DSAcq -> m r) -> m r

    default ds'Shape :: ( MonadSafe m
                       , Generic (d DSAcq)
                       , GDataSourceAcq (Rep (d DSAcq)))
                     => d DSAcq -> m DataSourceShape
    ds'Shape = generic'ds'Shape

    default withDataSourceP :: ( Generic (d DSPath)
                              , Generic (d DSAcq)
                              , GDataSourcePath (Rep (d DSPath)) (Rep (d DSAcq))
                              , Location l
                              , MonadSafe m
                              )
                            => ScanFile l -> d DSPath -> (d DSAcq -> m r) -> m r
    withDataSourceP = generic'withDataSourceP

withDataSourcesP :: (DataSource d, Location l, MonadSafe m, Show (d DSPath))
                 => ScanFile l -> [d DSPath] -> (d DSAcq -> m r) -> m r
withDataSourcesP f paths g = go paths
  where
    go [] = throwM $ HklDataSourceException'NoRemainingDataPath (show paths)
    go (s : ss) = withDataSourceP f s g
                  `catch`
                  \(_exl :: HklDataSourceException) -> go ss

-- DataSource (instances)

-- Attenuation

data family DSAttenuation (k :: DSKind)
data instance DSAttenuation DSPath
    = DataSourcePath'Attenuation { attenuationPath            :: DSWrap_ DSFloat DSPath
                                 , attenuationPathOffset      :: Int
                                 , attenuationPathCoefficient :: Double
                                 , attenuationPathMax         :: Maybe Float
                                 }
    | DataSourcePath'ApplyedAttenuationFactor { attenuationPath :: DSWrap_ DSFloat DSPath }
    | DataSourcePath'NoAttenuation
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data instance DSAttenuation DSAcq
    = DataSourceAcq'Attenuation { attenuationAcqPath        :: DSWrap_ DSFloat DSAcq
                                , attenuationAcqOffset      :: Int
                                , attenuationAcqCoefficient :: Double
                                , attenuationAcqMax         :: Maybe Float
                                }
    | DataSourceAcq'ApplyedAttenuationFactor { attenuationAcqPath :: DSWrap_ DSFloat DSAcq }
    | DataSourceAcq'NoAttenuation
    deriving (Generic)

instance DataSource DSAttenuation where
  ds'Shape (DataSourceAcq'Attenuation fp off _ _)
      = do (DataSourceShape'Range f (Z :. t)) <- ds'Shape fp
           pure $ DataSourceShape'Range f (Z :. (t - off))
  ds'Shape (DataSourceAcq'ApplyedAttenuationFactor fp) = ds'Shape fp
  ds'Shape DataSourceAcq'NoAttenuation                 = pure shape1


  withDataSourceP f (DataSourcePath'Attenuation p o c m) g = withDataSourcesP f p $ \ds -> g (DataSourceAcq'Attenuation ds o c m)
  withDataSourceP f (DataSourcePath'ApplyedAttenuationFactor p) g = withDataSourcesP f p $ \ds -> g (DataSourceAcq'ApplyedAttenuationFactor ds)
  withDataSourceP _ DataSourcePath'NoAttenuation g = g DataSourceAcq'NoAttenuation

-- Dataset

data family DSDataset (a :: Type) (k :: DSKind)
newtype instance DSDataset a DSPath
    = DataSourcePath'Dataset (Hdf5Path DIM1 a)
    deriving (Generic, Show, FromJSON, ToJSON)

newtype instance DSDataset a DSAcq
    = DataSourceAcq'Dataset Dataset

instance DataSource (DSDataset a) where
    ds'Shape (DataSourceAcq'Dataset ds) = liftIO $ ds'Shape'Dataset ds

    withDataSourceP (ScanFile f _) (DataSourcePath'Dataset p) g
        = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Dataset ds)

-- Degree

data family DSDegree (k :: DSKind)
data instance DSDegree DSPath
    = DataSourcePath'Degree (Hdf5Path DIM1 Double)
    | DataSourcePath'Degree'Const Degree
    deriving (Generic, Show, FromJSON, ToJSON)

data instance DSDegree DSAcq
    = DataSourceAcq'Degree Dataset
    | DataSourceAcq'Degree'Const Degree

instance DataSource DSDegree where
  ds'Shape (DataSourceAcq'Degree ds)      = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Degree'Const _) = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Degree p) g
    = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Degree ds)
  withDataSourceP _ (DataSourcePath'Degree'Const d) g = g (DataSourceAcq'Degree'Const d)

-- Double

data family DSDouble (k :: DSKind)
data instance DSDouble DSPath
    = DataSourcePath'Double (Hdf5Path Z Double)
    | DataSourcePath'Double'Ini ConfigContent Section Key
    | DataSourcePath'Double'Const Double
    deriving (Generic, Show, FromJSON, ToJSON)

data instance DSDouble DSAcq
    = DataSourceAcq'Double Dataset
    | DataSourceAcq'Double'Const Double

instance DataSource DSDouble where
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

-- [Double]

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

data family DSDoubles (k :: DSKind)
data instance DSDoubles DSPath
    = DataSourcePath'List [DSWrap_ DSDouble DSPath]
      deriving (Generic, FromJSON, Show, ToJSON)

data instance DSDoubles DSAcq
    = DataSourceAcq'List [DSWrap_ DSDouble DSAcq]

instance DataSource DSDoubles where
    ds'Shape  (DataSourceAcq'List ds)
        = do ss <- mapM ds'Shape ds
             pure $ foldl1 combine'Shape ss

    withDataSourceP f (DataSourcePath'List ps) g
        = nest (Prelude.map (withDataSourcesP f) ps) (\as -> g $ (DataSourceAcq'List as))

-- Float

data family DSFloat (k :: DSKind)
data instance DSFloat DSPath
    = DataSourcePath'Float (Hdf5Path DIM1 Float)
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
data instance DSFloat DSAcq
    = DataSourceAcq'Float Dataset

instance DataSource DSFloat where
    ds'Shape (DataSourceAcq'Float ds) = liftIO $ ds'Shape'Dataset ds

    withDataSourceP (ScanFile f _) (DataSourcePath'Float p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Float ds)

-- Geometry

data family DSGeometry (k :: DSKind)
data instance DSGeometry DSPath
    = DataSourcePath'Geometry { geometryGeometry       :: Geometry
                              , geometryPathWavelength :: DSWrap_ DSDouble DSPath
                              , geometryPathAxes       :: DSWrap_ DSDoubles DSPath
                              }
    | DataSourcePath'Geometry'Fix { geometryPathWavelength :: DSWrap_ DSDouble DSPath }
    deriving (Generic, Show, FromJSON, ToJSON)

data instance DSGeometry DSAcq
    = DataSourceAcq'Geometry
      Geometry
      (DSWrap_ DSDouble DSAcq)
      (DSWrap_ DSDoubles DSAcq)

instance DataSource DSGeometry where
  ds'Shape (DataSourceAcq'Geometry _ w as)
      = do sw <- ds'Shape w
           sas <- ds'Shape as
           pure $ sw `combine'Shape` sas

  withDataSourceP f (DataSourcePath'Geometry g w as) gg =
    withDataSourcesP f w $ \w' ->
    withDataSourcesP f as $ \as' -> do
    gg (DataSourceAcq'Geometry g w' as')
  withDataSourceP f (DataSourcePath'Geometry'Fix w) gg =
    withDataSourcesP f w $ \w' -> do
    gg (DataSourceAcq'Geometry fixed w' (DataSourceAcq'List []))

-- Image

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM []          = undefined
condM ((p, v):ls) = ifM p v (condM ls)

data family DSImage (k :: DSKind)

data instance DSImage DSPath
  = DataSourcePath'Image'Dummy (Detector Hkl DIM2) Double
  | DataSourcePath'Image'Hdf5 (Detector Hkl DIM2) (Hdf5Path DIM3 Int32) -- TODO Int32 is wrong
  | DataSourcePath'Image'Img (Detector Hkl DIM2) Text Scannumber
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSImage DSAcq
    = DataSourceAcq'Image'Dummy (IOVector Double)
    | DataSourceAcq'Image'Hdf5'Double (Detector Hkl DIM2) Dataset (IOVector Double)
    | DataSourceAcq'Image'Hdf5'Int32 (Detector Hkl DIM2) Dataset (IOVector Int32)
    | DataSourceAcq'Image'Hdf5'Word16 (Detector Hkl DIM2) Dataset (IOVector Word16)
    | DataSourceAcq'Image'Hdf5'Word32 (Detector Hkl DIM2) Dataset (IOVector Word32)
    | DataSourceAcq'Image'Img'Int32 (Detector Hkl DIM2) (IOVector Int32) Text Scannumber (Text -> Scannumber -> Int -> FilePath)

instance DataSource DSImage where
  ds'Shape (DataSourceAcq'Image'Dummy _)             = pure $ shape1
  ds'Shape (DataSourceAcq'Image'Hdf5'Double _ ds _)  = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Hdf5'Int32 _ ds _)   = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Hdf5'Word16 _ ds _)  = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Hdf5'Word32 _ ds _)  = liftIO $ ds'Shape'Dataset ds
  ds'Shape (DataSourceAcq'Image'Img'Int32 _ _ _ _ _) = pure $ shape1

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

  withDataSourceP (ScanFile _ sn) (DataSourcePath'Image'Img det tmpl (Scannumber sn0)) g
    = do let n = (size . shape $ det)
         arr <- liftIO $ unsafeNew n
         g (DataSourceAcq'Image'Img'Int32 det arr tmpl sn f)
             where
               f :: Text -> Scannumber -> Int -> FilePath
               f tmpl' (Scannumber sn') i = printf (unpack tmpl') sn0 sn0 ((sn' - sn0) * 1029 + i)

instance HasFieldValue [DSImage DSPath] where
    fieldvalue = autoJSON

-- Int

data family DSInt (k :: DSKind)
newtype instance DSInt DSPath
  = DataSourcePath'Int Int
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype instance DSInt DSAcq
  = DataSourceAcq'Int Int

instance DataSource DSInt where
  ds'Shape _ = pure $ shape1

  withDataSourceP _ (DataSourcePath'Int p) g = g (DataSourceAcq'Int p)

-- Mask

data family DSMask (k :: DSKind)
data instance DSMask DSPath
    = DataSourcePath'Mask'NoMask
    | DataSourcePath'Mask MaskLocation (Detector Hkl DIM2)
      deriving (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data instance DSMask DSAcq
    = DataSourceAcq'Mask Mask
    | DataSourceAcq'Mask'NoMask

instance DataSource DSMask where
    ds'Shape _ = pure $ shape1

    withDataSourceP _ DataSourcePath'Mask'NoMask g = g DataSourceAcq'Mask'NoMask
    withDataSourceP (ScanFile _ sn)  (DataSourcePath'Mask l d) g
        = do  m <- getMask l d sn
              g (DataSourceAcq'Mask m)

-- Scannumber

data family DSScannumber (k :: DSKind)
data instance DSScannumber DSPath = DataSourcePath'Scannumber
                                    deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSScannumber DSAcq = DataSourceAcq'Scannumber'Const Scannumber

instance DataSource DSScannumber where
  ds'Shape _ = pure $ shape1

  withDataSourceP (ScanFile _ s) DataSourcePath'Scannumber g = g (DataSourceAcq'Scannumber'Const s)


-- Timestamp

data family DSTimestamp (k :: DSKind)
data instance DSTimestamp DSPath
  = DataSourcePath'Timestamp (Hdf5Path DIM1 Double)
  | DataSourcePath'Timestamp'NoTimestamp
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSTimestamp DSAcq
    = DataSourceAcq'Timestamp Dataset
    | DataSourceAcq'Timestamp'NoTimestamp


instance DataSource DSTimestamp where
  ds'Shape (DataSourceAcq'Timestamp ds)        = liftIO $ ds'Shape'Dataset ds
  ds'Shape DataSourceAcq'Timestamp'NoTimestamp = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Timestamp p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Timestamp ds)
  withDataSourceP _ DataSourcePath'Timestamp'NoTimestamp g = g DataSourceAcq'Timestamp'NoTimestamp

-- Timescan0

data family DSTimescan0 (k :: DSKind)
data instance DSTimescan0 DSPath
    = DataSourcePath'Timescan0 (Hdf5Path DIM1 Double)
    | DataSourcePath'Timescan0'NoTimescan0
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSTimescan0 DSAcq
    = DataSourceAcq'Timescan0 Dataset
    | DataSourceAcq'Timescan0'NoTimescan0

instance DataSource DSTimescan0 where
  ds'Shape (DataSourceAcq'Timescan0 ds)        = liftIO $ ds'Shape'Dataset ds
  ds'Shape DataSourceAcq'Timescan0'NoTimescan0 = pure $ shape1

  withDataSourceP (ScanFile f _) (DataSourcePath'Timescan0 p) g = withHdf5PathP f p $ \ds -> g (DataSourceAcq'Timescan0 ds)
  withDataSourceP _ DataSourcePath'Timescan0'NoTimescan0 g = g DataSourceAcq'Timescan0'NoTimescan0
