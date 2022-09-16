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

module Hkl.Binoculars.Projections.QxQyQz
    ( Config(..)
    , DataPath(..)
    , DataFrameQxQyQz(..)
    , FramesQxQyQzP(..)
    , Resolutions
    , defaultDataPathQxQyQz
    , h5dpathQxQyQz
    , newQxQyQz
    , processQxQyQz
    , updateQxQyQz
    , withMaybeLimits
    , withDataPathQxQyQz
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
import           Pipes.Prelude                     (filter, map, print, tee,
                                                    toListM)
import           Pipes.Safe                        (MonadSafe, runSafeT)
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

--------------
-- DataPath --
--------------

data instance DataPath 'QxQyQzProjection = DataPathQxQyQz
  { dataPathQxQyQzAttenuation :: DataSourcePath Attenuation
  , dataPathQxQyQzGeometry :: DataSourcePath Geometry
  , dataPathQxQyQzDetector :: DataSourcePath Image
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

defaultDataPathQxQyQz :: DataPath 'QxQyQzProjection
defaultDataPathQxQyQz = DataPathQxQyQz
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

instance HasFieldValue (DataPath 'QxQyQzProjection) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

data instance Config 'QxQyQzProjection = BinocularsConfigQxQyQz
    { _binocularsConfigQxQyQzNcore                  :: Maybe Int
    , _binocularsConfigQxQyQzDestination            :: DestinationTmpl
    , _binocularsConfigQxQyQzOverwrite              :: Bool
    , _binocularsConfigQxQyQzInputType              :: InputType
    , _binocularsConfigQxQyQzNexusdir               :: Maybe (Path Abs Dir)
    , _binocularsConfigQxQyQzTmpl                   :: Maybe InputTmpl
    , _binocularsConfigQxQyQzInputRange             :: Maybe ConfigRange
    , _binocularsConfigQxQyQzDetector               :: Maybe (Detector Hkl DIM2)
    , _binocularsConfigQxQyQzCentralpixel           :: (Int, Int)
    , _binocularsConfigQxQyQzSdd                    :: Meter
    , _binocularsConfigQxQyQzDetrot                 :: Maybe Degree
    , _binocularsConfigQxQyQzAttenuationCoefficient :: Maybe Double
    , _binocularsConfigQxQyQzAttenuationMax         :: Maybe Float
    , _binocularsConfigQxQyQzSurfaceOrientation     :: Maybe SurfaceOrientation
    , _binocularsConfigQxQyQzMaskmatrix             :: Maybe MaskLocation
    , _binocularsConfigQxQyQzWavelength             :: Maybe Angstrom
    , _binocularsConfigQxQyQzProjectionType         :: ProjectionType
    , _binocularsConfigQxQyQzProjectionResolution   :: [Double]
    , _binocularsConfigQxQyQzProjectionLimits       :: Maybe [Limits]
    , _binocularsConfigQxQyQzDataPath               :: Maybe (DataPath 'QxQyQzProjection)
    , _binocularsConfigQxQyQzImageSumMax            :: Maybe Double
    } deriving (Eq, Show)

makeLenses 'BinocularsConfigQxQyQz

instance HasIniConfig 'QxQyQzProjection where

  defaultConfig = BinocularsConfigQxQyQz
    { _binocularsConfigQxQyQzNcore = Nothing
    , _binocularsConfigQxQyQzDestination = DestinationTmpl "."
    , _binocularsConfigQxQyQzOverwrite = False
    , _binocularsConfigQxQyQzInputType = SixsFlyScanUhv
    , _binocularsConfigQxQyQzNexusdir = Nothing
    , _binocularsConfigQxQyQzTmpl = Nothing
    , _binocularsConfigQxQyQzInputRange  = Nothing
    , _binocularsConfigQxQyQzDetector = Nothing
    , _binocularsConfigQxQyQzCentralpixel = (0, 0)
    , _binocularsConfigQxQyQzSdd = Meter (1 *~ meter)
    , _binocularsConfigQxQyQzDetrot = Nothing
    , _binocularsConfigQxQyQzAttenuationCoefficient = Nothing
    , _binocularsConfigQxQyQzAttenuationMax = Nothing
    , _binocularsConfigQxQyQzSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQxQyQzMaskmatrix = Nothing
    , _binocularsConfigQxQyQzWavelength = Nothing
    , _binocularsConfigQxQyQzProjectionType = QxQyQzProjection
    , _binocularsConfigQxQyQzProjectionResolution = [0.01, 0.01, 0.01]
    , _binocularsConfigQxQyQzProjectionLimits  = Nothing
    , _binocularsConfigQxQyQzDataPath = (Just defaultDataPathQxQyQz)
    , _binocularsConfigQxQyQzImageSumMax = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigQxQyQzNcore .=? field "ncores" auto
      binocularsConfigQxQyQzDestination .= field "destination" auto
      binocularsConfigQxQyQzOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigQxQyQzInputType .= field "type" auto
      binocularsConfigQxQyQzNexusdir .=? field "nexusdir" auto
      binocularsConfigQxQyQzTmpl .=? field "inputtmpl" auto
      binocularsConfigQxQyQzInputRange .=? field "inputrange" auto
      binocularsConfigQxQyQzDetector .=? field "detector" auto
      binocularsConfigQxQyQzCentralpixel .= field "centralpixel" auto
      binocularsConfigQxQyQzSdd .= field "sdd" auto
      binocularsConfigQxQyQzDetrot .=? field "detrot" auto
      binocularsConfigQxQyQzAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigQxQyQzAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigQxQyQzSurfaceOrientation .=? field "surface_orientation" auto
      binocularsConfigQxQyQzMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigQxQyQzWavelength .=? field "wavelength" auto
      binocularsConfigQxQyQzDataPath .=? field "datapath" auto
      binocularsConfigQxQyQzImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigQxQyQzProjectionType .= field "type" auto
      binocularsConfigQxQyQzProjectionResolution .= field "resolution" auto
      binocularsConfigQxQyQzProjectionLimits .=? field "limits" auto

  overwriteInputRange mr c = case mr of
                               Nothing  -> c
                               (Just _) -> c{_binocularsConfigQxQyQzInputRange = mr}


------------------
-- Input Path's --
------------------

data HklBinocularsProjectionsQxQyQzException
    = MissingAttenuationCoefficient
    deriving (Show)

instance Exception HklBinocularsProjectionsQxQyQzException

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


h5dpathQxQyQz ::  (MonadLogger m, MonadThrow m)
              => InputType
              -> Maybe Double
              -> Maybe Float
              -> Maybe (Detector Hkl DIM2)
              -> m (DataPath 'QxQyQzProjection)
h5dpathQxQyQz i ma mm mdet =
    do let det = fromMaybe defaultDetector mdet
       case i of
         CristalK6C -> DataPathQxQyQz
                      <$> mkAttenuation ma DataSourcePath'NoAttenuation
                      <*> pure (DataSourcePath'Geometry'CristalK6C
                                (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Monochromator" $ datasetp "lambda"))
                                (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-mu" $ datasetp "position"))
                                (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-komega" $ datasetp "position"))
                                (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-kappa" $ datasetp "position"))
                                (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "actuator_1_1"))
                                (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-gamma" $ datasetp "position"))
                                (DataSourcePath'Degree (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-delta" $ datasetp "position")))
                      <*> pure (DataSourcePath'Image
                                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_05")
                                det) -- medipix
         MarsFlyscan -> DataPathQxQyQz
                       <$> mkAttenuation ma (DataSourcePath'ApplyedAttenuationFactor
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "applied_att")))
                       <*> pure (DataSourcePath'Geometry'Mars
                                 (DataSourcePath'WaveLength'Const (Angstrom (1.537591 *~ angstrom)))
                                 [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                 , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi")
                                 , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi")
                                 , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth")
                                 ])
                       <*> pure (DataSourcePath'Image
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_quad_image"))
                                 det)
         MarsSbs -> DataPathQxQyQz
                   <$> mkAttenuation ma DataSourcePath'NoAttenuation
                   <*> pure (DataSourcePath'Geometry'Mars
                             (DataSourcePath'WaveLength'Const (Angstrom (1.537591 *~ angstrom)))
                             [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                             , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi")
                             , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi")
                             , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth")
                             ])
                   <*> pure (DataSourcePath'Image
                             (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image"))
                             det)
         SixsFlyMedH -> DataPathQxQyQz
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                             2 0 mm)
                       <*> pure (DataSourcePath'Geometry'MedH
                                 (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                 [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta") -- should be optional
                                 , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                 , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                 , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                 ])
                       <*> pure (DataSourcePath'Image
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image"))
                                 det)
         SixsFlyMedV -> DataPathQxQyQz
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                             2 0 mm)
                       <*> pure (DataSourcePath'Geometry'MedV
                                 (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                 (DataSourcePath'Degree'Const (Degree (0 *~ degree)))
                                 -- (DataSourcePath'Degree(H5Or
                                 --                         (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta")
                                 --                         (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch")))
                                 (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"))
                                 (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"))
                                 (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"))
                                 (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"))
                                 (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"))
                                )
                       <*> pure (DataSourcePath'Image
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                                 det)
         SixsFlyMedVEiger -> DataPathQxQyQz
                            <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                  (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                  2 0 mm)
                            <*> pure (DataSourcePath'Geometry'MedVEiger
                                      (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                      [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta") -- maybe nothing
                                      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa")
                                      ]
                                      (DataSourcePath'Degree(((hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                                             `H5Or`
                                                             (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre"))))
                                      (DataSourcePath'Degree(((hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                                             `H5Or`
                                                             (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre")))))
                            <*> pure (DataSourcePath'Image
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiger_image")
                                      det)
         SixsFlyMedVS70 -> DataPathQxQyQz
                          <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                2 0 mm)
                          <*> pure (DataSourcePath'Geometry'MedV
                                    (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                    (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta"))
                                    (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"))
                                    (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"))
                                    (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"))
                                    (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"))
                                    (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"))
                                   )
                          <*> pure (DataSourcePath'Image
                                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s70_image")
                                    det)
         SixsFlyScanUhv -> DataPathQxQyQz
                          <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                 2 0 mm)
                          <*> pure (DataSourcePath'Geometry'Uhv
                                    (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                                    [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                                    , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                                    ])
                          <*> pure (DataSourcePath'Image
                                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                    det)
         SixsFlyScanUhv2 -> DataPathQxQyQz
                           <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                 (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                 2 0 mm)
                           <*> pure (DataSourcePath'Geometry'Uhv
                                     (DataSourcePath'WaveLength (H5Or
                                                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                                                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")))
                                     [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                     , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                     , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                     , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                     ])
                           <*> pure (DataSourcePath'Image
                                     (H5Or
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image"))
                                     det)
         SixsFlyScanUhvTest -> DataPathQxQyQz
                           <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                 (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                 2 0 mm)
                           <*> pure (DataSourcePath'Geometry'UhvTest
                                     (DataSourcePath'WaveLength'Const (Angstrom (0.672494 *~ angstrom)))
                                     [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                     , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                     , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                     , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                     ])
                           <*> pure (DataSourcePath'Image
                                     (H5Or
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image"))
                                     det)
         SixsFlyScanUhvUfxc -> DataPathQxQyQz
                              <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                    (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation"))
                                                    2 0 mm)
                              <*> pure (DataSourcePath'Geometry'Uhv
                                        (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                                        [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                        , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                        ])
                              <*> pure (DataSourcePath'Image
                                        (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "ufxc_sixs_image")
                                        det)
         SixsSbsFixedDetector -> DataPathQxQyQz
                                <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                      (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                                      2 0 mm)
                                <*> pure (DataSourcePath'Geometry'Fix
                                          (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")))
                                <*> pure (DataSourcePath'Image
                                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_11")
                                          det)
         SixsSbsMedH -> DataPathQxQyQz
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                             0 0 mm)
                       <*> pure (DataSourcePath'Geometry'MedH
                                 (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                 [ DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch"))
                                 , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu"))
                                 , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma"))
                                 , DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta"))
                                 ])
                       <*> pure (DataSourcePath'Image
                                 (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image"))
                                 det)
         SixsSbsMedV -> DataPathQxQyQz
                       <$> mkAttenuation ma (DataSourcePath'Attenuation
                                             (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                             0 0 mm)
                       <*> pure (DataSourcePath'Geometry'MedV
                                 (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                 (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                                 (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                                 (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                                 (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                                 (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                                 (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")))
                                )
                       <*> pure (DataSourcePath'Image
                                 (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image"))
                                 det)
         SixsSbsMedVFixDetector -> DataPathQxQyQz
                                  <$> mkAttenuation ma (DataSourcePath'Attenuation
                                                        (DataSourcePath'Float (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att")))
                                                        0 0 mm)
                                  <*> pure (DataSourcePath'Geometry'MedV
                                            (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                            (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                                            (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                                            (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                                            (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                                            (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                                            (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")))
                                           )
                                  <*> pure (DataSourcePath'Image
                                            (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image"))
                                            det)

getResolution' :: MonadThrow m => Config 'QxQyQzProjection -> m [Double]
getResolution' c = getResolution (_binocularsConfigQxQyQzProjectionResolution c) 3

-----------------------
-- QxQyQz Projection --
-----------------------

type Resolutions = [Double]

data DataFrameQxQyQz
    = DataFrameQxQyQz
      Int -- n
      Attenuation -- attenuation
      Geometry -- geometry
      Image -- image
    deriving Show

data instance DataSourcePath DataFrameQxQyQz = DataSourcePath'DataFrameQxQyQz
                                               (DataSourcePath Int)
                                               (DataSourcePath Attenuation)
                                               (DataSourcePath Geometry)
                                               (DataSourcePath Image)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq DataFrameQxQyQz = DataSourceAcq'DataFrameQxQyQz
                                              (DataSourceAcq Int)
                                              (DataSourceAcq Attenuation)
                                              (DataSourceAcq Geometry)
                                              (DataSourceAcq Image)

instance DataSource DataFrameQxQyQz where
  withDataSourceP f (DataSourcePath'DataFrameQxQyQz j a g i) gg =
    withDataSourceP f j $ \j' ->
    withDataSourceP f a $ \a' ->
    withDataSourceP f g $ \g' ->
    withDataSourceP f i $ \i' -> gg (DataSourceAcq'DataFrameQxQyQz j' a' g' i')

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

{-# INLINE spaceQxQyQz #-}
spaceQxQyQz :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> SurfaceOrientation -> Maybe [Limits] -> Space DIM3 -> DataFrameQxQyQz -> IO (DataFrameSpace DIM3)
spaceQxQyQz det pixels rs mmask' surf mlimits space@(Space fSpace) (DataFrameQxQyQz _ att g img) =
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
      {-# SCC "hkl_binoculars_space_qxqyqz_int32_t" #-} c'hkl_binoculars_space_qxqyqz_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_uint16_t" #-} c'hkl_binoculars_space_qxqyqz_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_uint32_t" #-} c'hkl_binoculars_space_qxqyqz_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class ChunkP a => FramesQxQyQzP a where
  framesQxQyQzP :: MonadSafe m
                => a -> Pipe (FilePath, [Int]) DataFrameQxQyQz m ()

class (FramesQxQyQzP a, Show a) => ProcessQxQyQzP a where
  processQxQyQzP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QxQyQzProjection) m, MonadThrow m)
                 => m a -> m ()
  processQxQyQzP mkPaths = do
    (conf :: Config 'QxQyQzProjection) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigQxQyQzDetector conf)
    let mlimits = _binocularsConfigQxQyQzProjectionLimits conf
    let destination = _binocularsConfigQxQyQzDestination conf
    let output' = case _binocularsConfigQxQyQzInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> destination' (ConfigRange []) mlimits destination
    let centralPixel' = _binocularsConfigQxQyQzCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQxQyQzSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQxQyQzDetrot conf)
    let surfaceOrientation = fromMaybe SurfaceOrientationVertical (_binocularsConfigQxQyQzSurfaceOrientation conf)
    let mImageSumMax = _binocularsConfigQxQyQzImageSumMax conf

    h5d <- mkPaths
    filenames <- InputList
                <$> files (_binocularsConfigQxQyQzNexusdir conf)
                          (_binocularsConfigQxQyQzInputRange conf)
                          (_binocularsConfigQxQyQzTmpl conf)
    mask' <- getMask (_binocularsConfigQxQyQzMaskmatrix conf) det
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
      >-> tee Pipes.Prelude.print
      >-> framesQxQyQzP h5d
      >-> tee Pipes.Prelude.print
      >-> project det 3 (spaceQxQyQz det pixels res mask' surfaceOrientation mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a QxQyQz projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQxQyQzP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameQxQyQz _ _ _ img) ->
                                                           case mImageSumMax of
                                                             Nothing -> True
                                                             (Just m) -> if sumImage img < m
                                                                        then True
                                                                        else False
                                                       )
                               >-> project det 3 (spaceQxQyQz det pixels res mask' surfaceOrientation mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'


instance ProcessQxQyQzP (DataPath 'QxQyQzProjection)

instance ChunkP (DataPath 'QxQyQzProjection) where
    chunkP (DataPathQxQyQz ma _ (DataSourcePath'Image i _)) =
      skipMalformed $ forever $ do
      fp <- await
      withFileP (openH5 fp) $ \f ->
        withHdf5PathP f i $ \i' -> do
        (_, ss) <- liftIO $ datasetShape i'
        case head ss of
          (Just n) -> yield $ case ma of
            DataSourcePath'NoAttenuation             -> Chunk fp 0 (fromIntegral n - 1)
            (DataSourcePath'Attenuation _ off _ _) -> Chunk fp 0 (fromIntegral n - 1 - off)
            (DataSourcePath'ApplyedAttenuationFactor _) -> Chunk fp 0 (fromIntegral n -1)
          Nothing  -> error "can not extract length"

withDataPathQxQyQz :: (MonadSafe m, Location l) =>
                 l
               -> DataPath 'QxQyQzProjection
               -> ((Int -> IO DataFrameQxQyQz) -> m r)
               -> m r
withDataPathQxQyQz f (DataPathQxQyQz att det dif) g =
  withDataSourceP f att $ \att' ->
  withDataSourceP f det $ \det' ->
  withDataSourceP f dif $ \dif' ->
  g (\j -> DataFrameQxQyQz j
          <$> extract1DStreamValue att' j
          <*> extract1DStreamValue det' j
          <*> extract1DStreamValue dif' j)

instance FramesQxQyQzP (DataPath 'QxQyQzProjection) where
    framesQxQyQzP p =
        skipMalformed $ forever $ do
          (fn, js) <- await
          withFileP (openH5 fn) $ \f ->
            withDataPathQxQyQz f p $ \getDataFrameQxQyQz ->
            forM_ js (tryYield . getDataFrameQxQyQz)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'QxQyQzProjection) m)
         => m ()
process' = do
  c <- ask
  processQxQyQzP (h5dpathQxQyQz (_binocularsConfigQxQyQzInputType c) (_binocularsConfigQxQyQzAttenuationCoefficient c) (_binocularsConfigQxQyQzAttenuationMax c) (_binocularsConfigQxQyQzDetector c))

processQxQyQz :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
processQxQyQz mf mr = do
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

newQxQyQz :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Path Abs Dir -> m ()
newQxQyQz cwd = do
  let conf = defaultConfig {_binocularsConfigQxQyQzNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)

updateQxQyQz :: (MonadIO m, MonadLogger m, MonadThrow m)
             => Maybe FilePath -> m ()
updateQxQyQz mf = do
  (conf :: Either String (Config 'QxQyQzProjection)) <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)
