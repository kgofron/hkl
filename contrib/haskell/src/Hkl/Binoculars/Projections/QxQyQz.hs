{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
    ( DataFrameQxQyQz(..)
    , FramesQxQyQzP(..)
    , QxQyQzPath(..)
    , Resolutions
    , h5dpathQxQyQz
    , newQxQyQz
    , processQxQyQz
    , updateQxQyQz
    , withMaybeLimits
    , withQxQyQzPath
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
import           Data.Array.Repa                   (Array)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Ini.Config.Bidir             (field, ini, section,
                                                    serializeIni, (.=), (.=?))
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, pack)
import           Data.Text.IO                      (putStr)
import           Data.Typeable                     (typeOf)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr, nullPtr)
import           GHC.Conc                          (getNumCapabilities)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (Pipe, await, each,
                                                    runEffect, yield, (>->))
import           Pipes.Prelude                     (map, tee, toListM)
import           Pipes.Safe                        (MonadSafe, runSafeT)
import           Text.Printf                       (printf)


import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                        ()
import           Hkl.Pipes

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
    , _binocularsConfigQxQyQzSurfaceOrientation     :: Maybe SurfaceOrientation
    , _binocularsConfigQxQyQzMaskmatrix             :: Maybe MaskLocation
    , _binocularsConfigQxQyQzWavelength             :: Maybe Angstrom
    , _binocularsConfigQxQyQzProjectionType         :: ProjectionType
    , _binocularsConfigQxQyQzProjectionResolution   :: [Double]
    , _binocularsConfigQxQyQzProjectionLimits       :: Maybe [Limits]
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
    , _binocularsConfigQxQyQzSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQxQyQzMaskmatrix = Nothing
    , _binocularsConfigQxQyQzWavelength = Nothing
    , _binocularsConfigQxQyQzProjectionType = QxQyQzProjection
    , _binocularsConfigQxQyQzProjectionResolution = [0.01, 0.01, 0.01]
    , _binocularsConfigQxQyQzProjectionLimits  = Nothing
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
      binocularsConfigQxQyQzSurfaceOrientation .=? field "surface_orientation" auto
      binocularsConfigQxQyQzMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigQxQyQzWavelength .=? field "wavelength" auto
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

mkAttenuation :: (MonadLogger m, MonadThrow m) => Maybe Double -> AttenuationPath -> m AttenuationPath
mkAttenuation ma att =
    case ma of
      Nothing -> case att of
                  NoAttenuation     -> return NoAttenuation
                  AttenuationPath{} -> do
                           $(logWarn) ("The current configuration extract the attenuation from the data files." :: Text)
                           logWarnN "You forgot to provide the attenuation coefficient in the config file."
                           logWarnN ("I continue without attenuation correction" :: Text)
                           logWarnN ("Add attenuation_coefficient=<something> under the [input] section, to fix this" :: Text)
                           return NoAttenuation
                  applyed@ApplyedAttenuationFactorPath{} -> return applyed
      (Just coef) -> return $ case att of
                               NoAttenuation           -> NoAttenuation
                               (AttenuationPath p o _) -> AttenuationPath p o coef
                               (ApplyedAttenuationFactorPath _) -> undefined


h5dpathQxQyQz ::  (MonadLogger m, MonadThrow m)
              => InputType
              -> Maybe Double
              -> m QxQyQzPath
h5dpathQxQyQz i ma =
    do case i of
         CristalK6C -> QxQyQzPath
                      <$> mkAttenuation ma NoAttenuation
                      <*> pure (DetectorPath
                                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_05")) -- medipix
                      <*> pure (GeometryPathCristalK6C
                                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Monochromator" $ datasetp "lambda")
                                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-mu" $ datasetp "position")
                                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-komega" $ datasetp "position")
                                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-kappa" $ datasetp "position")
                                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "actuator_1_1")
                                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-gamma" $ datasetp "position")
                                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-delta" $ datasetp "position"))
         MarsFlyscan -> QxQyQzPath
                       <$> mkAttenuation ma (ApplyedAttenuationFactorPath
                                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "applied_att"))
                       <*> pure (DetectorPath
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_quad_image")))
                       <*> pure (GeometryPathMars
                                 [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth"
                                 ])
         MarsSbs -> QxQyQzPath
                   <$> mkAttenuation ma NoAttenuation
                   <*> pure (DetectorPath
                             (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image")))
                   <*> pure (GeometryPathMars
                             [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                             , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi"
                             , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi"
                             , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth"
                             ])
         SixsFlyMedH -> QxQyQzPath
                       <$> mkAttenuation ma (AttenuationPath
                                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                            2 0)
                       <*> pure (DetectorPath
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image")))
                       <*> pure (GeometryPathMedH
                                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                 [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta" -- should be optional
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                                 ])
         SixsFlyMedV -> QxQyQzPath
                       <$> mkAttenuation ma (AttenuationPath
                                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                            2 0)
                       <*> pure (DetectorPath
                                 (H5Or
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                  (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image")))
                       <*> pure (GeometryPathMedV
                                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                 [ -- hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta" it was not saved in the file
                                   hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"
                                 ])
         SixsFlyMedVEiger -> QxQyQzPath
                            <$> mkAttenuation ma (AttenuationPath
                                                 (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                                 2 0)
                            <*> pure (DetectorPath
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiger_image"))
                            <*> pure (GeometryPathMedVEiger
                                      (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                      [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta" -- maybe nothing
                                      , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                                      , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                                      , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                                      , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                                      , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"
                                      ]
                                      ((hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                       `H5Or`
                                       (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre"))
                                      ((hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                       `H5Or`
                                       (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre")))
         SixsFlyMedVS70 -> QxQyQzPath
                          <$> mkAttenuation ma (AttenuationPath
                                               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                               2 0)
                          <*> pure (DetectorPath
                                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s70_image"))
                          <*> pure (GeometryPathMedV
                                    (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                    [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"
                                    ])
         SixsFlyScanUhv -> QxQyQzPath
                          <$> mkAttenuation ma (AttenuationPath
                                               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                               2 0)
                          <*> pure (DetectorPath
                                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                          <*> pure (GeometryPathUhv
                                    (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                                    [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA"
                                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA"
                                    ])
         SixsFlyScanUhv2 -> QxQyQzPath
                           <$> mkAttenuation ma (AttenuationPath
                                                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                                2 0)
                           <*> pure (DetectorPath
                                     (H5Or
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_s140_image")))
                           <*> pure (GeometryPathUhv
                                     (H5Or
                                      (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                                      (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                                     [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                                     ])
         SixsFlyScanUhvUfxc -> QxQyQzPath
                              <$> mkAttenuation ma (AttenuationPath
                                                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                                   2 0)
                              <*> pure (DetectorPath
                                        (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "ufxc_sixs_image"))
                              <*> pure (GeometryPathUhv
                                        (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                                        [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                                        , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                                        , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                                        , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                                        ])
         SixsSbsFixedDetector -> QxQyQzPath
                                <$> mkAttenuation ma (AttenuationPath
                                                     (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                                     2 0)
                                <*> pure (DetectorPath
                                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_11"))
                                <*> pure (GeometryPathFix
                                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
         SixsSbsMedH -> QxQyQzPath
                       <$> mkAttenuation ma (AttenuationPath
                                            (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                            0 0)
                       <*> pure (DetectorPath
                                 (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
                       <*> pure (GeometryPathMedH
                                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                 [ hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta")
                                 ])
         SixsSbsMedV -> QxQyQzPath
                       <$> mkAttenuation ma (AttenuationPath
                                            (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                            0 0)
                       <*> pure (DetectorPath
                                 (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
                       <*> pure (GeometryPathMedV
                                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                 [ hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")
                                 , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")
                                 ])
         SixsSbsMedVFixDetector -> QxQyQzPath
                                  <$> mkAttenuation ma (AttenuationPath
                                                       (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                                       0 0)
                                  <*> pure (DetectorPath
                                            (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image")))
                                  <*> pure (GeometryPathMedV
                                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                                            [ hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"
                                            , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")
                                            , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")
                                            , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")
                                            , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")
                                            , hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")
                                            ])

getResolution' :: MonadThrow m => Config 'QxQyQzProjection -> m [Double]
getResolution' c = getResolution (_binocularsConfigQxQyQzProjectionResolution c) 3

-----------------------
-- QxQyQz Projection --
-----------------------

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
    (ImageInt32 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_int32_t" #-} c'hkl_binoculars_space_qxqyqz_int32_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord16 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_uint16_t" #-} c'hkl_binoculars_space_qxqyqz_uint16_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord32 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_uint32_t" #-} c'hkl_binoculars_space_qxqyqz_uint32_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class ChunkP a => FramesQxQyQzP a where
  framesQxQyQzP :: MonadSafe m
                => a -> Detector b DIM2 -> Pipe (FilePath, [Int]) DataFrameQxQyQz m ()

class (FramesQxQyQzP a, Show a) => ProcessQxQyQzP a where
  processQxQyQzP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QxQyQzProjection) m, MonadThrow m)
                 => m a -> m ()
  processQxQyQzP mkPaths = do
    (conf :: Config 'QxQyQzProjection) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigQxQyQzDetector conf)
    let output' = case _binocularsConfigQxQyQzInputRange conf of
                   Just r  -> destination' r (_binocularsConfigQxQyQzDestination conf)
                   Nothing -> destination' (ConfigRange []) (_binocularsConfigQxQyQzDestination conf)
    let centralPixel' = _binocularsConfigQxQyQzCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQxQyQzSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQxQyQzDetrot conf)
    let surfaceOrientation = fromMaybe SurfaceOrientationVertical (_binocularsConfigQxQyQzSurfaceOrientation conf)
    let mlimits = _binocularsConfigQxQyQzProjectionLimits conf

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
    $(logDebug) "start gessing final cube size"

    -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

    guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
      runSafeT $ runEffect $
      each chunks
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, (quot (f + t) 4), (quot (f + t) 4) * 2, (quot (f + t) 4) * 3, t]))
      >-> framesQxQyQzP h5d det
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
                               >-> framesQxQyQzP h5d det
                               -- >-> filter (\(DataFrameQxQyQz _ _ _ ma) -> isJust ma)
                               >-> project det 3 (spaceQxQyQz det pixels res mask' surfaceOrientation mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'


instance ProcessQxQyQzP QxQyQzPath

instance ChunkP QxQyQzPath where
    chunkP (QxQyQzPath ma (DetectorPath i) _) =
      skipMalformed $ forever $ do
      fp <- await
      withFileP (openH5 fp) $ \f ->
        withHdf5PathP f i $ \i' -> do
        (_, ss) <- liftIO $ datasetShape i'
        case head ss of
          (Just n) -> yield $ case ma of
            NoAttenuation             -> Chunk fp 0 (fromIntegral n - 1)
            (AttenuationPath _ off _) -> Chunk fp 0 (fromIntegral n - 1 - off)
            (ApplyedAttenuationFactorPath _) -> Chunk fp 0 (fromIntegral n -1)
          Nothing  -> error "can not extract length"

withQxQyQzPath :: (MonadSafe m, Location l) =>
                 l
               -> Detector a DIM2
               -> QxQyQzPath
               -> ((Int -> IO DataFrameQxQyQz) -> m r)
               -> m r
withQxQyQzPath f det (QxQyQzPath att d dif) g =
  withAttenuationPathP f att $ \getAttenuation ->
  withDetectorPathP f det d $ \getImage ->
  withGeometryPathP f dif $ \getDiffractometer ->
  g (\j -> DataFrameQxQyQz j
          <$> getAttenuation j
          <*> getDiffractometer j
          <*> getImage j
    )

instance FramesQxQyQzP QxQyQzPath where
    framesQxQyQzP p det =
        skipMalformed $ forever $ do
          (fn, js) <- await
          withFileP (openH5 fn) $ \f ->
            withQxQyQzPath f det p $ \getDataFrameQxQyQz ->
            forM_ js (tryYield . getDataFrameQxQyQz)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'QxQyQzProjection) m)
         => m ()
process' = do
  c <- ask
  processQxQyQzP (h5dpathQxQyQz (_binocularsConfigQxQyQzInputType c) (_binocularsConfigQxQyQzAttenuationCoefficient c))

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
