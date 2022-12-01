{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QCustom
    ( Config(..)
    , DataPath(..)
    , DataFrameQCustom(..)
    , FramesQCustomP(..)
    , Resolutions
    , defaultDataPathQCustom
    , h5dpathQCustom
    , newQCustom
    , processQCustom
    , updateQCustom
    , withMaybeLimits
    , withDataPathQCustom
    ) where

import           Bindings.HDF5.Core                (Location)
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Lens                      (makeLenses)
import           Control.Monad                     (zipWithM)
import           Control.Monad.Catch               (Exception, MonadThrow)
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
import           Data.Ini.Config.Bidir             (FieldValue (..), field, ini,
                                                    section, serializeIni, (.=),
                                                    (.=?))
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, pack)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           Data.Text.IO                      (putStr)
import           Data.Vector.Storable.Mutable      (unsafeWith)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr, nullPtr)
import           GHC.Conc                          (getNumCapabilities)
import           GHC.Generics                      (Generic)
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
import           Hkl.C.Geometry
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                        ()
import           Hkl.Pipes
import           Hkl.Types

--------------
-- DataPath --
--------------

data instance DataPath 'QCustomProjection = DataPathQCustom
  { dataPathQCustomAttenuation :: DataSourcePath Attenuation
  , dataPathQCustomGeometry :: DataSourcePath Geometry
  , dataPathQCustomDetector :: DataSourcePath Image
  , dataPathQCustonTimestamp :: DataSourcePath Index
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance Arbitrary (DataPath 'QCustomProjection) where
  arbitrary = DataPathQCustom <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

defaultDataPathQCustom :: DataPath 'QCustomProjection
defaultDataPathQCustom = DataPathQCustom
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
                          (defaultDetector))
                        (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))

instance HasFieldValue (DataPath 'QCustomProjection) where
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
    , _binocularsConfigQCustomProjectionResolution   :: [Double]
    , _binocularsConfigQCustomProjectionLimits       :: Maybe [Limits]
    , _binocularsConfigQCustomDataPath               :: Maybe (DataPath 'QCustomProjection)
    , _binocularsConfigQCustomImageSumMax            :: Maybe Double
    , _binocularsConfigQCustomSubProjection          :: Maybe QCustomSubProjection
    } deriving (Eq, Show)

makeLenses 'BinocularsConfigQCustom

instance HasIniConfig 'QCustomProjection where

  defaultConfig = BinocularsConfigQCustom
    { _binocularsConfigQCustomNcore = Nothing
    , _binocularsConfigQCustomDestination = DestinationTmpl "."
    , _binocularsConfigQCustomOverwrite = False
    , _binocularsConfigQCustomInputType = SixsFlyScanUhv
    , _binocularsConfigQCustomNexusdir = Nothing
    , _binocularsConfigQCustomTmpl = Nothing
    , _binocularsConfigQCustomInputRange  = Nothing
    , _binocularsConfigQCustomDetector = Nothing
    , _binocularsConfigQCustomCentralpixel = (0, 0)
    , _binocularsConfigQCustomSdd = Meter (1 *~ meter)
    , _binocularsConfigQCustomDetrot = Nothing
    , _binocularsConfigQCustomAttenuationCoefficient = Nothing
    , _binocularsConfigQCustomAttenuationMax = Nothing
    , _binocularsConfigQCustomSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQCustomMaskmatrix = Nothing
    , _binocularsConfigQCustomWavelength = Nothing
    , _binocularsConfigQCustomProjectionType = QCustomProjection
    , _binocularsConfigQCustomProjectionResolution = [0.01, 0.01, 0.01]
    , _binocularsConfigQCustomProjectionLimits  = Nothing
    , _binocularsConfigQCustomDataPath = (Just defaultDataPathQCustom)
    , _binocularsConfigQCustomImageSumMax = Nothing
    , _binocularsConfigQCustomSubProjection = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigQCustomNcore .=? field "ncores" auto
      binocularsConfigQCustomDestination .= field "destination" auto
      binocularsConfigQCustomOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigQCustomInputType .= field "type" auto
      binocularsConfigQCustomNexusdir .=? field "nexusdir" auto
      binocularsConfigQCustomTmpl .=? field "inputtmpl" auto
      binocularsConfigQCustomInputRange .=? field "inputrange" auto
      binocularsConfigQCustomDetector .=? field "detector" auto
      binocularsConfigQCustomCentralpixel .= field "centralpixel" auto
      binocularsConfigQCustomSdd .= field "sdd" auto
      binocularsConfigQCustomDetrot .=? field "detrot" auto
      binocularsConfigQCustomAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigQCustomAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigQCustomSurfaceOrientation .=? field "surface_orientation" auto
      binocularsConfigQCustomMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigQCustomWavelength .=? field "wavelength" auto
      binocularsConfigQCustomDataPath .=? field "datapath" auto
      binocularsConfigQCustomImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigQCustomProjectionType .= field "type" auto
      binocularsConfigQCustomProjectionResolution .= field "resolution" auto
      binocularsConfigQCustomProjectionLimits .=? field "limits" auto
      binocularsConfigQCustomSubProjection .=? field "subprojection" auto

  overwriteInputRange mr c = case mr of
                               Nothing  -> c
                               (Just _) -> c{_binocularsConfigQCustomInputRange = mr}


------------------
-- Input Path's --
------------------

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
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

mkWaveLength :: (MonadLogger m, MonadThrow m) => Maybe Angstrom -> DataSourcePath WaveLength -> m (DataSourcePath WaveLength)
mkWaveLength ma wp =
    case ma of
      Nothing  -> return wp
      (Just a) -> return $ DataSourcePath'WaveLength'Const a

mkTimeStamp :: (MonadLogger m, MonadThrow m) => Maybe QCustomSubProjection -> DataSourcePath Index -> m (DataSourcePath Index)
mkTimeStamp msub idx =
  case msub of
    Nothing -> return DataSourcePath'Index'NoIndex
    (Just sub) -> return $ case sub of
                   QCustomSubProjection'QxQyQz -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QTthTimestamp -> idx

h5dpathQCustom ::  (MonadLogger m, MonadThrow m)
              => InputType
              -> Maybe Double
              -> Maybe Float
              -> Maybe (Detector Hkl DIM2)
              -> Maybe Angstrom
              -> Maybe QCustomSubProjection
              -> m (DataPath 'QCustomProjection)
h5dpathQCustom i ma mm mdet mw msub =
    do let det = fromMaybe defaultDetector mdet
       case i of
         CristalK6C -> DataPathQCustom
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
         MarsFlyscan -> DataPathQCustom
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
         MarsSbs -> DataPathQCustom
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
         SixsFlyMedH -> DataPathQCustom
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                             2 0 mm)
                       <*> (DataSourcePath'Geometry'MedH
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta") -- should be optional
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                    ])
                       <*> pure (DataSourcePath'Image
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image"))
                                 det)
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyMedV -> DataPathQCustom
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                             2 0 mm)
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
                       <*> pure (DataSourcePath'Image
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                                 det)
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyMedVEiger -> DataPathQCustom
                            <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                  (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                  2 0 mm)
                            <*> (DataSourcePath'Geometry'MedVEiger
                                <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta") -- maybe nothing
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                         , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa")
                                         ]
                                <*> pure (DataSourcePath'Degree(((hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                                                 `H5Or`
                                                                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre"))))
                                <*> pure (DataSourcePath'Degree(((hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                                                 `H5Or`
                                                                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre")))))
                            <*> pure (DataSourcePath'Image
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiger_image")
                                      det)
                            <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyMedVS70 -> DataPathQCustom
                          <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                2 0 mm)
                          <*> (DataSourcePath'Geometry'MedV
                              <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"))
                              <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"))
                              )
                          <*> pure (DataSourcePath'Image
                                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s70_image")
                                    det)
                          <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhv -> DataPathQCustom
                          <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                 2 0 mm)
                          <*> (DataSourcePath'Geometry'Uhv
                              <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                              <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                                       ])
                          <*> pure (DataSourcePath'Image
                                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                    det)
                          <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhv2 -> DataPathQCustom
                           <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                 (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                 2 0 mm)
                           <*> (DataSourcePath'Geometry'Uhv
                               <$> mkWaveLength mw (DataSourcePath'WaveLength (H5Or
                                                                               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                                                                               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")))
                               <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                        ])
                           <*> pure (DataSourcePath'Image
                                     (H5Or
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                      (H5Or
                                        (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image")
                                        (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s70_image")))
                                     det)
                           <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhvTest -> DataPathQCustom
                           <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                 (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                 2 0 mm)
                           <*> (DataSourcePath'Geometry'UhvTest
                               <$> mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (0.672494 *~ angstrom)))
                               <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                        ])
                           <*> pure (DataSourcePath'Image
                                     (H5Or
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image"))
                                     det)
                           <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsFlyScanUhvUfxc -> DataPathQCustom
                              <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                    (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                    2 0 mm)
                              <*> (DataSourcePath'Geometry'Uhv
                                  <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                                  <*> pure [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                           , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                           , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                           , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                           ])
                              <*> pure (DataSourcePath'Image
                                        (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "ufxc_sixs_image")
                                        det)
                              <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))
         SixsSbsFixedDetector -> DataPathQCustom
                                <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                      (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                                      2 0 mm)
                                <*> (DataSourcePath'Geometry'Fix
                                    <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")))
                                <*> pure (DataSourcePath'Image
                                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_11")
                                          det)
                                <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsMedH -> DataPathQCustom
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                             0 0 mm)
                       <*> (DataSourcePath'Geometry'MedH
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure [ DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch"))
                                    , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu"))
                                    , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma"))
                                    , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta"))
                                    ])
                       <*> pure (DataSourcePath'Image
                                 (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image"))
                                 det)
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsMedV -> DataPathQCustom
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                             0 0 mm)
                       <*> (DataSourcePath'Geometry'MedV
                           <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                           <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")))
                           )
                       <*> pure (DataSourcePath'Image
                                 (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image"))
                                 det)
                       <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))
         SixsSbsMedVFixDetector -> DataPathQCustom
                                  <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                        (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                                        0 0 mm)
                                  <*> (DataSourcePath'Geometry'MedV
                                      <$> mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                                      <*> pure (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")))
                                      )
                                  <*> pure (DataSourcePath'Image
                                            (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image"))
                                            det)
                                  <*> mkTimeStamp msub (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))

getResolution' :: MonadThrow m => Config 'QCustomProjection -> m [Double]
getResolution' c = getResolution (_binocularsConfigQCustomProjectionResolution c) 3

-----------------------
-- QCustom Projection --
-----------------------

type Resolutions = [Double]

data DataFrameQCustom
    = DataFrameQCustom
      Int -- n
      Attenuation -- attenuation
      Geometry -- geometry
      Image -- image
      Index -- timestamp in double
    deriving Show

data instance DataSourcePath DataFrameQCustom = DataSourcePath'DataFrameQCustom
                                               (DataSourcePath Int)
                                               (DataSourcePath Attenuation)
                                               (DataSourcePath Geometry)
                                               (DataSourcePath Image)
                                               (DataSourcePath Index)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq DataFrameQCustom = DataSourceAcq'DataFrameQCustom
                                              (DataSourceAcq Int)
                                              (DataSourceAcq Attenuation)
                                              (DataSourceAcq Geometry)
                                              (DataSourceAcq Image)
                                              (DataSourceAcq Index)

instance DataSource DataFrameQCustom where
  withDataSourceP f (DataSourcePath'DataFrameQCustom j a g i idx) gg =
    withDataSourceP f j $ \j' ->
    withDataSourceP f a $ \a' ->
    withDataSourceP f g $ \g' ->
    withDataSourceP f i $ \i' ->
    withDataSourceP f idx $ \idx' -> gg (DataSourceAcq'DataFrameQCustom j' a' g' i' idx')

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

{-# INLINE spaceQCustom #-}
spaceQCustom :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> SurfaceOrientation -> Maybe [Limits] -> QCustomSubProjection -> Space DIM3 -> DataFrameQCustom -> IO (DataFrameSpace DIM3)
spaceQCustom det pixels rs mmask' surf mlimits subprojection space@(Space fSpace) (DataFrameQCustom _ att g img index) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withArrayLen rs $ \nr r ->
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
    let det = fromMaybe defaultDetector (_binocularsConfigQCustomDetector conf)
    let mlimits = _binocularsConfigQCustomProjectionLimits conf
    let destination = _binocularsConfigQCustomDestination conf
    let output' = case _binocularsConfigQCustomInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> destination' (ConfigRange []) mlimits destination
    let centralPixel' = _binocularsConfigQCustomCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQCustomSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQCustomDetrot conf)
    let surfaceOrientation = fromMaybe SurfaceOrientationVertical (_binocularsConfigQCustomSurfaceOrientation conf)
    let mImageSumMax = _binocularsConfigQCustomImageSumMax conf
    let subprojection = fromMaybe QCustomSubProjection'QxQyQz (_binocularsConfigQCustomSubProjection conf)

    h5d <- mkPaths
    filenames <- InputList
                <$> files (_binocularsConfigQCustomNexusdir conf)
                          (_binocularsConfigQCustomInputRange conf)
                          (_binocularsConfigQCustomTmpl conf)
    mask' <- getMask (_binocularsConfigQCustomMaskmatrix conf) det
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot
    res <- getResolution' conf

    -- compute the jobs

    let fns = concatMap (replicate 1) (toList filenames)
    chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP h5d
    cap' <-  liftIO $ getNumCapabilities
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
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, (quot (f + t) 4), (quot (f + t) 4) * 2, (quot (f + t) 4) * 3, t]))
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
                               >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ _ img _) -> filterSumImage mImageSumMax img)
                               >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'


instance ProcessQCustomP (DataPath 'QCustomProjection)

instance ChunkP (DataPath 'QCustomProjection) where
    chunkP (DataPathQCustom ma _ (DataSourcePath'Image i _) _) =
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

withDataPathQCustom :: (MonadSafe m, Location l) =>
                 l
               -> DataPath 'QCustomProjection
               -> ((Int -> IO DataFrameQCustom) -> m r)
               -> m r
withDataPathQCustom f (DataPathQCustom att det dif timestamp) g =
  withDataSourceP f att $ \att' ->
  withDataSourceP f det $ \det' ->
  withDataSourceP f dif $ \dif' ->
  withDataSourceP f timestamp $ \timestamp' ->
  g (\j -> DataFrameQCustom j
          <$> extract1DStreamValue att' j
          <*> extract1DStreamValue det' j
          <*> extract1DStreamValue dif' j
          <*> extract1DStreamValue timestamp' j
    )

instance FramesQCustomP (DataPath 'QCustomProjection) where
    framesQCustomP p =
        skipMalformed $ forever $ do
          (fn, js) <- await
          withFileP (openH5 fn) $ \f ->
            withDataPathQCustom f p $ \getDataFrameQCustom ->
            forM_ js (tryYield . getDataFrameQCustom)

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

processQCustom :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
processQCustom mf mr = do
  econf <- liftIO $ getConfig mf
  case econf of
    Right conf -> do
      $(logDebug) "config red from the config file"
      $(logDebugSH) conf
      let conf' = overwriteInputRange mr conf
      $(logDebug) "config once overloaded with the command line arguments"
      $(logDebugSH) conf'
      runReaderT process' conf'
    Left e      -> $(logErrorSH) e

newQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Path Abs Dir -> m ()
newQCustom cwd = do
  let conf = defaultConfig {_binocularsConfigQCustomNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)

updateQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
             => Maybe FilePath -> m ()
updateQCustom mf = do
  (conf :: Either String (Config 'QCustomProjection)) <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)
