{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-
    Copyright  : Copyright (C) 2014-2021 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Sixs
  (process) where

import           Control.Monad.Catch        (Exception, MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (MonadLogger, logDebugSH,
                                             logErrorSH, logWarn, logWarnN)
import           Data.Text                  (Text)

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.H5                     hiding (File)


data HklBinocularsSixsException
    = MissingAttenuationCoefficient
    | MissingSampleParameters BinocularsConfig
    deriving (Show)

instance Exception HklBinocularsSixsException

mkAttenuation :: (MonadLogger m, MonadThrow m) => BinocularsConfig -> AttenuationPath -> m AttenuationPath
mkAttenuation c att = case _binocularsInputAttenuationCoefficient c of
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


h5dpathQxQyQz ::  (MonadLogger m, MonadThrow m) => BinocularsConfig -> m QxQyQzPath
h5dpathQxQyQz c = case _binocularsInputItype c of
  CristalK6C -> QxQyQzPath
               <$> mkAttenuation c NoAttenuation
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
                <$> mkAttenuation c (ApplyedAttenuationFactorPath
                                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "applied_att"))
                <*> pure (DetectorPath
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "merlin_image"))
                <*> pure (GeometryPathMars
                          [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                          , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "chi"
                          , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "phi"
                          , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "tth"
                          ])
  SixsFlyMedH -> QxQyQzPath
                  <$> mkAttenuation c (AttenuationPath
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
                  <$> mkAttenuation c (AttenuationPath
                                       (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                       2 0)
                  <*> pure (DetectorPath
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
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
                  <$> mkAttenuation c (AttenuationPath
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
                  <$> mkAttenuation c (AttenuationPath
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
                   <$> mkAttenuation c (AttenuationPath
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
                    <$> mkAttenuation c (AttenuationPath
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
                       <$> mkAttenuation c (AttenuationPath
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
                         <$> mkAttenuation c (AttenuationPath
                                              (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                              2 0)
                         <*> pure (DetectorPath
                                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_11"))
                         <*> pure (GeometryPathFix
                                   (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
  SixsSbsMedH -> QxQyQzPath
                  <$> mkAttenuation c (AttenuationPath
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
                <$> mkAttenuation c (AttenuationPath
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
                <$> mkAttenuation c (AttenuationPath
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

--  FramesHklP

h5dpathHkl :: (MonadLogger m, MonadThrow m) => BinocularsConfig -> m HklPath
h5dpathHkl c = do
    let samplePath beamline device =
          SamplePath
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "A")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "B")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "C")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Ux")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uy")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uz")
    let sampleMarsPath beamline device =
          SamplePath
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "a")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "b")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "c")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_x")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_y")
          (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_z")
    let marsSamplePath = sampleMarsPath "MARS" "d03-1-cx2__ex__dif-cm_#1"
    let medHSamplePath = samplePath "SIXS" "i14-c-cx1-ex-cm-med.h"
    let medVSamplePath = samplePath "SIXS" "i14-c-cx1-ex-cm-med.v"
    let uhvSamplePath  = samplePath "SIXS" "I14-C-CX2__EX__DIFF-UHV__#1"
    let uhvSamplePath2 = samplePath "SIXS" "i14-c-cx2-ex-diff-uhv"
    qxqyqz <- h5dpathQxQyQz c
    case _binocularsInputItype c of
      CristalK6C -> do
                 let ms = sampleConfig c
                 case ms of
                   (Just s) -> return (HklPath qxqyqz (SamplePath2 s))
                   Nothing  -> throwM (MissingSampleParameters c)
      MarsFlyscan -> return $ HklPath qxqyqz marsSamplePath
      SixsFlyMedH -> return $ HklPath qxqyqz medHSamplePath
      SixsFlyMedV -> return $ HklPath qxqyqz medVSamplePath
      SixsFlyMedVEiger -> return $ HklPath qxqyqz medVSamplePath
      SixsFlyMedVS70 -> return $ HklPath qxqyqz medVSamplePath
      SixsFlyScanUhv -> return $ HklPath qxqyqz uhvSamplePath
      SixsFlyScanUhv2 -> return $ HklPath qxqyqz uhvSamplePath2
      SixsFlyScanUhvUfxc -> return $ HklPath qxqyqz uhvSamplePath
      SixsSbsFixedDetector -> undefined -- TODO this must not be possible.
      SixsSbsMedH -> return $ HklPath qxqyqz medHSamplePath
      SixsSbsMedV -> return $ HklPath qxqyqz medVSamplePath
      SixsSbsMedVFixDetector -> return $ HklPath qxqyqz medVSamplePath

         -- SixsSbsMedV -> HklPath
         --               hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")  -- xpad
         --               (GeometryPath
         --                hdf5p -- TODO wavelength
         --                [ hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch" -- beta
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")
         --                ])
         --               medVSamplePath
         --               -- "attenuation": DatasetPathWithAttribute("long_name", b"i14-c-c00/ex/roic/att"),
         --               -- "timestamp": HItem("sensors_timestamps", True),
process :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
process mf mr = do
  conf <- liftIO $ getConfig mf
  $(logDebugSH) conf
  case conf of
    Right conf' -> do
              let c = combineWithCmdLineArgs conf' mr
              $(logDebugSH) c
              case _binocularsProjectionPtype c of
                 QxQyQzProjection -> processQxQyQzP c h5dpathQxQyQz
                 HklProjection    -> processHklP c h5dpathHkl
    Left e   -> $(logErrorSH) e
