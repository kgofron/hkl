{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QCustom
    ( Config(..)
    , DataFrameQCustom(..)
    , FramesQCustomP(..)
    , defaultDataSourcePath'DataFrameQCustom
    , h5dpathQCustom
    , newQCustom
    , processQCustom
    , updateQCustom
    ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Lens                      (Lens, makeLenses, set',
                                                    (^.))
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger, logDebug,
                                                    logDebugSH, logErrorSH,
                                                    logInfo, logWarn, logWarnN)
import           Control.Monad.Reader              (MonadReader, ask, forM_,
                                                    forever)
import           Control.Monad.Trans.Reader        (runReaderT)
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    eitherDecode', encode)
import           Data.Array.Repa                   (Array)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import           Data.HashMap.Lazy                 (fromList)
import           Data.Ini                          (Ini (..))
import           Data.Ini.Config                   (fieldMbOf, fieldOf, section)
import           Data.Ini.Config.Bidir             (FieldValue (..))
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Text                         (Text, pack)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           Data.Text.IO                      (putStr)
import           Data.Vector.Storable.Mutable      (unsafeWith)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           GHC.Conc                          (getNumCapabilities)
import           GHC.Generics                      (Generic)
import           Generic.Random                    (genericArbitraryU)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (Pipe, await, each,
                                                    runEffect, yield, (>->))
import           Pipes.Prelude                     (filter, map, tee, toListM)
import           Pipes.Safe                        (MonadSafe, runSafeT)
import           Test.QuickCheck                   (Arbitrary (..))
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                        ()
import           Hkl.Pipes
import           Hkl.Types

-----------------------
-- QCustom Projection --
-----------------------

data DataFrameQCustom
    = DataFrameQCustom
      Attenuation -- attenuation
      Geometry -- geometry
      Image -- image
      Index -- timestamp in double
    deriving Show

data instance DataSourcePath DataFrameQCustom
  = DataSourcePath'DataFrameQCustom
    (DataSourcePath Attenuation)
    (DataSourcePath Geometry)
    (DataSourcePath Image)
    (DataSourcePath Index)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq DataFrameQCustom
  = DataSourceAcq'DataFrameQCustom
    (DataSourceAcq Attenuation)
    (DataSourceAcq Geometry)
    (DataSourceAcq Image)
    (DataSourceAcq Index)

instance Arbitrary (DataSourcePath DataFrameQCustom) where
  arbitrary = DataSourcePath'DataFrameQCustom <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance DataSource DataFrameQCustom where
  withDataSourceP f (DataSourcePath'DataFrameQCustom a g i idx) gg =
    withDataSourceP f a $ \a' ->
    withDataSourceP f g $ \g' ->
    withDataSourceP f i $ \i' ->
    withDataSourceP f idx $ \idx' -> gg (DataSourceAcq'DataFrameQCustom a' g' i' idx')

instance Is1DStreamable (DataSourceAcq DataFrameQCustom) DataFrameQCustom where
    extract1DStreamValue (DataSourceAcq'DataFrameQCustom att geom img idx) i =
      DataFrameQCustom
      <$> extract1DStreamValue att i
      <*> extract1DStreamValue geom i
      <*> extract1DStreamValue img i
      <*> extract1DStreamValue idx i


defaultDataSourcePath'DataFrameQCustom :: DataSourcePath DataFrameQCustom
defaultDataSourcePath'DataFrameQCustom
  = DataSourcePath'DataFrameQCustom
    (DataSourcePath'Attenuation
      (DataSourcePath'Float (hdf5p $ grouppat 0 $ datasetp "scan_data/attenuation"))
      2 0 Nothing)
    (DataSourcePath'Geometry'Uhv
      (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ datasetp "SIXS/Monochromator/wavelength"))
      [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_MU")
      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_OMEGA")
      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_DELTA")
      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_GAMMA")
      ])
    (DataSourcePath'Image
      (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_image")
      defaultDetector)
    (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))

instance HasFieldValue (DataSourcePath DataFrameQCustom) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

data instance Config 'QCustomProjection = BinocularsConfigQCustom
    { _binocularsConfigQCustomNcore                  :: Maybe Int
    , _binocularsConfigQCustomDestination            :: DestinationTmpl
    , _binocularsConfigQCustomOverwrite              :: Bool
    , _binocularsConfigQCustomInputType              :: InputType
    , _binocularsConfigQCustomNexusdir               :: Maybe (Path Abs Dir)
    , _binocularsConfigQCustomTmpl                   :: Maybe InputTmpl
    , _binocularsConfigQCustomInputRange             :: Maybe ConfigRange
    , _binocularsConfigQCustomDetector               :: Maybe (Detector Hkl DIM2)
    , _binocularsConfigQCustomCentralpixel           :: (Int, Int)
    , _binocularsConfigQCustomSdd                    :: Meter
    , _binocularsConfigQCustomDetrot                 :: Maybe Degree
    , _binocularsConfigQCustomAttenuationCoefficient :: Maybe Double
    , _binocularsConfigQCustomAttenuationMax         :: Maybe Float
    , _binocularsConfigQCustomSurfaceOrientation     :: Maybe SurfaceOrientation
    , _binocularsConfigQCustomMaskmatrix             :: Maybe MaskLocation
    , _binocularsConfigQCustomWavelength             :: Maybe Angstrom
    , _binocularsConfigQCustomProjectionType         :: ProjectionType
    , _binocularsConfigQCustomProjectionResolution   :: Resolutions DIM3
    , _binocularsConfigQCustomProjectionLimits       :: Maybe (RLimits DIM3)
    , _binocularsConfigQCustomDataPath               :: Maybe (DataSourcePath DataFrameQCustom)
    , _binocularsConfigQCustomImageSumMax            :: Maybe Double
    , _binocularsConfigQCustomSubProjection          :: Maybe QCustomSubProjection
    } deriving (Eq, Show, Generic)

makeLenses 'BinocularsConfigQCustom

instance Arbitrary (Config 'QCustomProjection) where
  arbitrary = genericArbitraryU

instance HasIniConfig 'QCustomProjection where

  defaultConfig = BinocularsConfigQCustom
    { _binocularsConfigQCustomNcore = Nothing
    , _binocularsConfigQCustomDestination = DestinationTmpl "."
    , _binocularsConfigQCustomOverwrite = False
    , _binocularsConfigQCustomInputType = SixsFlyScanUhv
    , _binocularsConfigQCustomNexusdir = Nothing
    , _binocularsConfigQCustomTmpl = Nothing
    , _binocularsConfigQCustomInputRange  = Nothing
    , _binocularsConfigQCustomDetector = Just defaultDetector
    , _binocularsConfigQCustomCentralpixel = (0, 0)
    , _binocularsConfigQCustomSdd = Meter (1 *~ meter)
    , _binocularsConfigQCustomDetrot = Just (Degree (0 *~ degree))
    , _binocularsConfigQCustomAttenuationCoefficient = Nothing
    , _binocularsConfigQCustomAttenuationMax = Nothing
    , _binocularsConfigQCustomSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQCustomMaskmatrix = Nothing
    , _binocularsConfigQCustomWavelength = Nothing
    , _binocularsConfigQCustomProjectionType = QCustomProjection
    , _binocularsConfigQCustomProjectionResolution = Resolutions3 0.01 0.01 0.01
    , _binocularsConfigQCustomProjectionLimits  = Nothing
    , _binocularsConfigQCustomDataPath = Just defaultDataSourcePath'DataFrameQCustom
    , _binocularsConfigQCustomImageSumMax = Nothing
    , _binocularsConfigQCustomSubProjection = Just QCustomSubProjection'QxQyQz
    }

  specConfig = undefined

  overwriteWithCmd mr conf
    = overload mr conf
      >>= sanitizeSubProjection
      >>= overloadMaybeDefault binocularsConfigQCustomDetector
      >>= overloadMaybeDefault binocularsConfigQCustomDetrot
      >>= overloadMaybeDefault binocularsConfigQCustomSurfaceOrientation
      >>= overloadMaybeDefault binocularsConfigQCustomSubProjection

    where
      -- overload with command line
      overload :: MonadLogger m
               => Maybe ConfigRange -> Config 'QCustomProjection -> m (Config 'QCustomProjection)
      overload mr' conf'
        = case mr' of
            Nothing  -> pure conf'
            r -> do
               $(logDebug) "overloading config range with the command line arguments:"
               $(logDebugSH) r
               pure conf' {_binocularsConfigQCustomInputRange = r}

      -- sanitize due to the evolution of the projection implementation
      sanitizeSubProjection :: MonadLogger m
                            => Config 'QCustomProjection -> m (Config 'QCustomProjection)
      sanitizeSubProjection conf'
        = case _binocularsConfigQCustomProjectionType conf of
            QxQyQzProjection   -> do
              $(logDebug) "overwrite subprojection QCustomSubProjection'QxQyQz for the QxQyQzProjection input type"
              pure conf'{_binocularsConfigQCustomSubProjection = Just QCustomSubProjection'QxQyQz}
            _ -> pure conf'

      -- set default values if relevant
      overloadMaybeDefault :: (MonadLogger m, Show a)
                           => Lens (Config 'QCustomProjection) (Config 'QCustomProjection) (Maybe a) (Maybe a)
                           -> Config 'QCustomProjection -> m (Config 'QCustomProjection)
      overloadMaybeDefault l conf'
        = case conf' ^. l of
            Nothing  -> do
              let mv = defaultConfig ^. l
              $(logDebug) "using default value:"
              $(logDebugSH) (fromJust mv)
              pure $ set' l mv conf'
            (Just _) -> pure conf'

  configParser = do
    (ncores, destination, overwrite) <-
      Data.Ini.Config.section "dispatcher" $ do
      (,,)
        <$> fieldMbOf "ncores" auto'
        <*> fieldOf   "destination" auto'
        <*> fieldOf   "overwrite" auto'

    (inputtype, nexusdir, inputtmpl, inputrange, detector, centralpixel, sdd, detrot, attenuation_coefficient, attenuation_max, surface_orientation, maskmatrix, wavelength, datapath, image_sum_max) <-
      Data.Ini.Config.section "input" $ do
      (,,,,,,,,,,,,,,)
        <$> fieldOf   "type" auto'
        <*> fieldMbOf "nexusdir" auto'
        <*> fieldMbOf "inputtmpl" auto'
        <*> fieldMbOf "inputrange" auto'
        <*> fieldMbOf "detector" auto'
        <*> fieldOf   "centralpixel" auto'
        <*> fieldOf   "sdd" auto'
        <*> fieldMbOf "detrot" auto'
        <*> fieldMbOf "attenuation_coefficient" auto'
        <*> fieldMbOf "attenuation_max" auto'
        <*> fieldMbOf "surface_orientation" auto'
        <*> fieldMbOf "maskmatrix" auto'
        <*> fieldMbOf "wavelength" auto'
        <*> fieldMbOf "datapath" auto'
        <*> fieldMbOf "image_sum_max" auto'

    (projectiontype, resolution, limits, subprojection) <-
      Data.Ini.Config.section "projection" $ do
      (,,,)
        <$> fieldOf   "type" auto'
        <*> fieldOf   "resolution" auto'
        <*> fieldMbOf "limits" auto'
        <*> fieldMbOf "subprojection" auto'

    pure $ BinocularsConfigQCustom ncores destination overwrite inputtype nexusdir inputtmpl inputrange detector centralpixel sdd detrot attenuation_coefficient attenuation_max surface_orientation maskmatrix wavelength projectiontype resolution limits datapath image_sum_max subprojection

  toIni c = Ini { iniSections = ss
                , iniGlobals = []
                }
    where
      otua :: HasFieldValue a => a -> Text
      otua = fvEmit fieldvalue

      elem :: HasFieldValue a => Text -> a -> [(Text, Text)]
      elem k v = [(k, otua v)]

      elemMb :: HasFieldValue a => Text -> Maybe a -> [(Text, Text)]
      elemMb k mv = case mv of
                      (Just v) -> [(k, otua v)]
                      Nothing  -> []

      ss = fromList [ ("dispatcher", elemMb  "ncores" (_binocularsConfigQCustomNcore c)
                                     <> elem "destination" (_binocularsConfigQCustomDestination c)
                                     <> elem "overwrite" (_binocularsConfigQCustomOverwrite c)
                      )
                    ,  ("input", elem      "type" (_binocularsConfigQCustomInputType c)
                                 <> elemMb "nexusdir" (_binocularsConfigQCustomNexusdir c)
                                 <> elemMb "inputtmpl" (_binocularsConfigQCustomTmpl c)
                                 <> elemMb "inputrange" (_binocularsConfigQCustomInputRange c)
                                 <> elemMb "detector" (_binocularsConfigQCustomDetector c)
                                 <> elem   "centralpixel" (_binocularsConfigQCustomCentralpixel c)
                                 <> elem   "sdd" (_binocularsConfigQCustomSdd c)
                                 <> elemMb "detrot" (_binocularsConfigQCustomDetrot c)
                                 <> elemMb "attenuation_coefficient" (_binocularsConfigQCustomAttenuationCoefficient c)
                                 <> elemMb "attenuation_max" (_binocularsConfigQCustomAttenuationMax c)
                                 <> elemMb "surface_orientation" (_binocularsConfigQCustomSurfaceOrientation c)
                                 <> elemMb "maskmatrix" (_binocularsConfigQCustomMaskmatrix c)
                                 <> elemMb "wavelength" (_binocularsConfigQCustomWavelength c)
                                 <> elemMb "datapath" (_binocularsConfigQCustomDataPath c)
                                 <> elemMb "image_sum_max" (_binocularsConfigQCustomImageSumMax c)
                       )
                    , ("projection", elem      "type" (_binocularsConfigQCustomProjectionType c)
                                     <> elem   "resolution" (_binocularsConfigQCustomProjectionResolution c)
                                     <> elemMb "limits" (_binocularsConfigQCustomProjectionLimits c)
                                     <> elemMb "subprojection" (_binocularsConfigQCustomSubProjection c)
                      )]

------------------
-- Input Path's --
------------------

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsQCustomException


mkAttenuation :: (MonadLogger m, MonadThrow m) => Maybe Double -> DataSourcePath Attenuation -> m (DataSourcePath Attenuation)
mkAttenuation ma att =
    case ma of
      Nothing -> case att of
                  DataSourcePath'NoAttenuation     -> return DataSourcePath'NoAttenuation
                  DataSourcePath'Attenuation{} -> do
                           $(logWarn) ("The current configuration extract the attenuation from the data files." :: Text)
                           logWarnN "You forgot to provide the attenuation coefficient in the config file."
                           logWarnN ("I continue without attenuation correction" :: Text)
                           logWarnN ("Add attenuation_coefficient=<something> under the [input] section, to fix this" :: Text)
                           return DataSourcePath'NoAttenuation
                  applyed@DataSourcePath'ApplyedAttenuationFactor{} -> return applyed
      (Just coef) -> return $ case att of
                               DataSourcePath'NoAttenuation           -> DataSourcePath'NoAttenuation
                               (DataSourcePath'Attenuation p o _ m) -> DataSourcePath'Attenuation p o coef m
                               (DataSourcePath'ApplyedAttenuationFactor _) -> undefined

mkDetector'Sixs'Fly :: (MonadLogger m, MonadThrow m) => Detector Hkl DIM2 -> m (DataSourcePath Image)
mkDetector'Sixs'Fly det@(Detector2D n _ _)
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S140 =
      pure (DataSourcePath'Image
            (H5Or
             (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
             (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image"))
            det)
  | n == c'HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S70 =
      pure (DataSourcePath'Image
            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s70_image")
            det)
  | n == c'HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M =
      pure (DataSourcePath'Image
            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiger_image")
            det)
  | n == c'HKL_BINOCULARS_DETECTOR_UFXC =
      pure (DataSourcePath'Image
            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "ufxc_sixs_image")
            det)
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD = undefined
  | otherwise = undefined

mkDetector'Sixs'Sbs :: (MonadLogger m, MonadThrow m) => Detector Hkl DIM2 -> m (DataSourcePath Image)
mkDetector'Sixs'Sbs det@(Detector2D n _ _)
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S140 =
      pure (DataSourcePath'Image
            (H5Or
             (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.s140/image"))
             (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
            det)
  | n == c'HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S70 =
      pure (DataSourcePath'Image
            (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.s70/image"))
            det)
  | n == c'HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M =
      pure (DataSourcePath'Image
             (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image"))
             det)
  | n == c'HKL_BINOCULARS_DETECTOR_UFXC = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD = undefined
  | otherwise = undefined

mkTimeStamp :: (MonadLogger m, MonadThrow m) => Maybe QCustomSubProjection -> DataSourcePath Index -> m (DataSourcePath Index)
mkTimeStamp msub idx =
  case msub of
    Nothing -> return DataSourcePath'Index'NoIndex
    (Just sub) -> return $ case sub of
                   QCustomSubProjection'QxQyQz -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QTthTimestamp -> idx
                   QCustomSubProjection'QparQperTimestamp -> idx
                   QCustomSubProjection'QPhiQx -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QPhiQy -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QPhiQz -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QStereo -> DataSourcePath'Index'NoIndex

mkWaveLength :: (MonadLogger m, MonadThrow m) => Maybe Angstrom -> DataSourcePath WaveLength -> m (DataSourcePath WaveLength)
mkWaveLength ma wp =
    case ma of
      Nothing  -> return wp
      (Just a) -> return $ DataSourcePath'WaveLength'Const a


h5dpathQCustom ::  (MonadLogger m, MonadThrow m)
              => InputType
              -> Maybe Double
              -> Maybe Float
              -> Maybe (Detector Hkl DIM2)
              -> Maybe Angstrom
              -> Maybe QCustomSubProjection
              -> m (DataSourcePath DataFrameQCustom)
h5dpathQCustom i ma mMaxAtt mdet mw msub =
    do let det = fromMaybe defaultDetector mdet
       let dataSourcePath'Attenuation'Sixs :: DataSourcePath Attenuation
           dataSourcePath'Attenuation'Sixs =
             DataSourcePath'Attenuation
             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" (H5Or
                                                                              (datasetp "attenuation")
                                                                              (datasetp "attenuation_old")
                                                                            )))
             2 0 mMaxAtt
       let dataSourcePath'Attenuation'SixsSBS :: DataSourcePath Attenuation
           dataSourcePath'Attenuation'SixsSBS =
             DataSourcePath'Attenuation
             (DataSourcePath'Float (hdf5p (H5Or
                                            (datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                            (H5Or
                                              (datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att"))
                                              (H5Or
                                                (datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att_old"))
                                                (H5Or
                                                  (datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att"))
                                                  (datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att_old"))))))))
             0 0 mMaxAtt

       case i of
         CristalK6C -> DataSourcePath'DataFrameQCustom
                      <$> mkAttenuation ma DataSourcePath'NoAttenuation
                      <*> (DataSourcePath'Geometry'CristalK6C
                          <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Monochromator" $ datasetp "lambda"))
                          <*> pure (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-mu" $ datasetp "position"))
                          <*> pure (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-komega" $ datasetp "position"))
                          <*> pure (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-kappa" $ datasetp "position"))
                          <*> pure (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "actuator_1_1"))
                          <*> pure (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-gamma" $ datasetp "position"))
                          <*> pure (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-delta" $ datasetp "position")))
                      <*> pure (DataSourcePath'Image
                                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_05")
                                det) -- medipix
                      <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         MarsFlyscan -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma (DataSourcePath'ApplyedAttenuationFactor
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "applied_att")))
                       <*> (DataSourcePath'Geometry'Mars
                           <$> mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (1.537591 *~ angstrom)))
                           <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth")
                                    ])
                       <*> pure (DataSourcePath'Image
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_quad_image"))
                                 det)
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         MarsSbs -> DataSourcePath'DataFrameQCustom
                   <$> mkAttenuation ma DataSourcePath'NoAttenuation
                   <*> (DataSourcePath'Geometry'Mars
                       <$> mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (1.537591 *~ angstrom)))
                       <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi")
                                , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi")
                                , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth")
                                ])
                   <*> pure (DataSourcePath'Image
                             (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image"))
                             det)
                   <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsFlyMedH -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                       <*> (DataSourcePath'Geometry'MedH
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta") -- should be optional
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                    ])
                       <*> mkDetector'Sixs'Fly det
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyMedV -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                       <*> (DataSourcePath'Geometry'MedV
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure (DataSourcePath'Degree'Const (Degree (0 *~ degree)))
                           -- (DataSourcePath'Degree(H5Or
                           --                         (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta")
                           --                         (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"))
                           )
                       <*> mkDetector'Sixs'Fly det
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyMedVEiger -> DataSourcePath'DataFrameQCustom
                            <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                            <*> (DataSourcePath'Geometry'MedVEiger
                                <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta") -- maybe nothing
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa")
                                         ]
                                <*> pure (DataSourcePath'Degree(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                                                `H5Or`
                                                                hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre")))
                                  <*> pure (DataSourcePath'Degree(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                                                  `H5Or`
                                                                  hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre"))))
                            <*> mkDetector'Sixs'Fly det
                            <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyMedVS70 -> DataSourcePath'DataFrameQCustom
                          <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                          <*> (DataSourcePath'Geometry'MedV
                              <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"))
                              )
                          <*> mkDetector'Sixs'Fly det
                          <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhv -> DataSourcePath'DataFrameQCustom
                          <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                          <*> (DataSourcePath'Geometry'Uhv
                              <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                              <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                                       ])
                          <*> mkDetector'Sixs'Fly det
                          <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhv2 -> DataSourcePath'DataFrameQCustom
                           <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                           <*> (DataSourcePath'Geometry'Uhv
                               <$> mkWaveLength mw (DataSourcePath'WaveLength (H5Or
                                                                               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                                                                               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")))
                               <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                        ])
                           <*> mkDetector'Sixs'Fly det
                           <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhvTest -> DataSourcePath'DataFrameQCustom
                              <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                              <*> (DataSourcePath'Geometry'UhvTest
                                   <$> mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (0.672494 *~ angstrom)))
                                   <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                            , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                            , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                            , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                            ])
                              <*> mkDetector'Sixs'Fly det
                              <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhvUfxc -> DataSourcePath'DataFrameQCustom
                              <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                              <*> (DataSourcePath'Geometry'Uhv
                                  <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                                  <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                           , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                           , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                           , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                           ])
                              <*> mkDetector'Sixs'Fly det
                              <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsSbsFixedDetector -> DataSourcePath'DataFrameQCustom
                                <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                                <*> (DataSourcePath'Geometry'Fix
                                    <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")))
                                <*> mkDetector'Sixs'Fly det
                                <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsMedH -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                       <*> (DataSourcePath'Geometry'MedH
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure [ DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch"))
                                    , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu"))
                                    , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma"))
                                    , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta"))
                                    ])
                       <*> mkDetector'Sixs'Sbs det
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsMedV -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                       <*> (DataSourcePath'Geometry'MedV
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")))
                           )
                       <*> mkDetector'Sixs'Sbs det
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsMedVFixDetector -> DataSourcePath'DataFrameQCustom
                                  <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                                  <*> (DataSourcePath'Geometry'MedV
                                      <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")))
                                      )
                                  <*> mkDetector'Sixs'Sbs det
                                  <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsUhv -> DataSourcePath'DataFrameQCustom
                      <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                      <*> (DataSourcePath'Geometry'Uhv
                            <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                            <*> pure [ DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/mu"))
                                     , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/omega"))
                                     , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/delta"))
                                     , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/gamma"))
                                     ])
                      <*> mkDetector'Sixs'Sbs det
                      <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))

{-# INLINE spaceQCustom #-}
spaceQCustom :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> SurfaceOrientation -> Maybe (RLimits DIM3) -> QCustomSubProjection -> Space DIM3 -> DataFrameQCustom -> IO (DataFrameSpace DIM3)
spaceQCustom det pixels rs mmask' surf mlimits subprojection space@(Space fSpace) (DataFrameQCustom att g img index) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_int32_t" #-} c'hkl_binoculars_space_qcustom_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint16_t" #-} c'hkl_binoculars_space_qcustom_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint32_t" #-} c'hkl_binoculars_space_qcustom_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class ChunkP a => FramesQCustomP a where
  framesQCustomP :: MonadSafe m
                => a -> Pipe (FilePath, [Int]) DataFrameQCustom m ()

class (FramesQCustomP a, Show a) => ProcessQCustomP a where
  processQCustomP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QCustomProjection) m, MonadThrow m)
                 => m a -> m ()
  processQCustomP mkPaths = do
    (conf :: Config 'QCustomProjection) <- ask

    -- should not be Maybe
    let det = fromJust (_binocularsConfigQCustomDetector conf)
    let (Degree detrot) = fromJust ( _binocularsConfigQCustomDetrot conf)
    let surfaceOrientation = fromJust (_binocularsConfigQCustomSurfaceOrientation conf)
    let subprojection = fromJust (_binocularsConfigQCustomSubProjection conf)

    -- directly from the config
    let mlimits = _binocularsConfigQCustomProjectionLimits conf
    let destination = _binocularsConfigQCustomDestination conf
    let centralPixel' = _binocularsConfigQCustomCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQCustomSdd conf
    let mImageSumMax = _binocularsConfigQCustomImageSumMax conf
    let res = _binocularsConfigQCustomProjectionResolution conf

    -- built from the config
    let output' = case _binocularsConfigQCustomInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> throwM MissingInputRange

    h5d <- mkPaths
    filenames <- InputFn'List
                <$> files (_binocularsConfigQCustomNexusdir conf)
                          (_binocularsConfigQCustomInputRange conf)
                          (_binocularsConfigQCustomTmpl conf)
    mask' <- getMask (_binocularsConfigQCustomMaskmatrix conf) det
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

    -- compute the jobs

    let fns = concatMap (replicate 1) (toList filenames)
    chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP h5d
    cap' <-  liftIO getNumCapabilities
    let ntot = sum (Prelude.map clength chunks)
    let cap = if cap' >= 2 then cap' - 1 else cap'
    let jobs = chunk (quot ntot cap) chunks

    -- log parameters

    $(logDebugSH) filenames
    $(logDebugSH) h5d
    $(logDebugSH) chunks
    $(logDebug) "start gessing final cube size"

    -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

    guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
      runSafeT $ runEffect $
      each chunks
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t]))
      >-> framesQCustomP h5d
      >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a QCustom projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQCustomP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                               >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'


instance ProcessQCustomP (DataSourcePath DataFrameQCustom)

instance ChunkP (DataSourcePath DataFrameQCustom) where
    chunkP (DataSourcePath'DataFrameQCustom ma _ (DataSourcePath'Image i _) _) =
      skipMalformed $ forever $ do
      fp <- await
      withFileP (openH5 fp) $ \f ->
        withHdf5PathP f i $ \i' -> do
        (_, ss) <- liftIO $ datasetShape i'
        case head ss of
          (Just n) -> yield $ case ma of
            DataSourcePath'NoAttenuation -> Chunk fp 0 (fromIntegral n - 1)
            (DataSourcePath'Attenuation _ off _ _) -> Chunk fp 0 (fromIntegral n - 1 - off)
            (DataSourcePath'ApplyedAttenuationFactor _) -> Chunk fp 0 (fromIntegral n -1)
          Nothing  -> error "can not extract length"

instance FramesQCustomP (DataSourcePath DataFrameQCustom) where
    framesQCustomP p =
        skipMalformed $ forever $ do
          (fn, js) <- await
          withFileP (openH5 fn) $ \f ->
            withDataSourceP f p $ \ g ->
            forM_ js (tryYield . extract1DStreamValue g)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'QCustomProjection) m)
         => m ()
process' = do
  c <- ask
  processQCustomP (h5dpathQCustom
                  (_binocularsConfigQCustomInputType c)
                  (_binocularsConfigQCustomAttenuationCoefficient c)
                  (_binocularsConfigQCustomAttenuationMax c)
                  (_binocularsConfigQCustomDetector c)
                  (_binocularsConfigQCustomWavelength c)
                  (_binocularsConfigQCustomSubProjection c)
                 )

processQCustom :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQCustom mf mr = do
  econf <- getConfig' mf
  case econf of
    Right conf -> do
      $(logDebug) "config red from the config file"
      $(logDebugSH) conf
      $(logDebug) ""

      conf' <- overwriteWithCmd mr conf

      $(logDebug) "config once validated"
      $(logDebugSH) conf'
      $(logDebug) ""

      runReaderT process' conf'
    Left e      -> $(logErrorSH) e

newQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Path Abs Dir -> m ()
newQCustom cwd = do
  let conf = defaultConfig {_binocularsConfigQCustomNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
              => Maybe FilePath -> m ()
updateQCustom mf = do
  (conf :: Either String (Config 'QCustomProjection)) <- getConfig' mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeConfig conf'
