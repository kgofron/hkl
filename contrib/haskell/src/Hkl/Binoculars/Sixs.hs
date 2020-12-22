{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
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

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.H5                     hiding (File)


data HklBinocularsSixsException
    = MissingAttenuationCoefficient
    | MissingSampleParameters BinocularsConfig
    deriving (Show)

instance Exception HklBinocularsSixsException

mkAttenuation :: MonadThrow m => BinocularsConfig -> AttenuationPath -> m AttenuationPath
mkAttenuation c att = case _binocularsInputAttenuationCoefficient c of
                        Nothing -> case att of
                                    NoAttenuation -> return NoAttenuation
                                    AttenuationPath{}             -> throwM MissingAttenuationCoefficient
                        (Just coef) -> return $ case att of
                                        NoAttenuation -> NoAttenuation
                                        (AttenuationPath p o _) -> AttenuationPath p o coef

h5dpathQxQyQz :: MonadThrow m => BinocularsConfig -> m QxQyQzPath
h5dpathQxQyQz c = case _binocularsInputItype c of
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
                              (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                    <*> pure (GeometryPathUhv
                              (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                              [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                              , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                              , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                              , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                              ])
  SixsSbsMedV -> QxQyQzPath
                <$> mkAttenuation c (AttenuationPath
                                      (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                      0 0)
                <*> pure (DetectorPath
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                <*> pure (GeometryPathUhv
                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                          [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                          , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                          , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                          , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                          ])
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

--  FramesHklP

h5dpathHkl :: MonadThrow m => BinocularsConfig -> m HklPath
h5dpathHkl c = do
    let sixsSample device = SamplePath
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "A")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "B")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "C")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "alpha")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "beta")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "gamma")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "Ux")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "Uy")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp device $ datasetp "Uz")
    let uhvSamplePath = sixsSample "I14-C-CX2__EX__DIFF-UHV__#1"
    let cmMedVSamplePath = sixsSample "i14-c-cx1-ex-cm-med.v"
    qxqyqz <- h5dpathQxQyQz c
    case _binocularsInputItype c of
      SixsFlyMedV     -> return $ HklPath qxqyqz cmMedVSamplePath
      SixsFlyScanUhv  -> return $ HklPath qxqyqz uhvSamplePath
      SixsFlyScanUhv2 -> return $ HklPath qxqyqz uhvSamplePath
      SixsSbsMedV     -> return $ HklPath qxqyqz cmMedVSamplePath
      CristalK6C      -> do
                 let ms = sampleConfig c
                 case ms of
                   (Just s) -> return (HklPath qxqyqz (SamplePath2 s))
                   Nothing  -> throwM (MissingSampleParameters c)

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
process :: Maybe FilePath -> Maybe (ConfigRange Int) -> IO ()
process mf mr = do
  conf <- getConfig mf
  case conf of
    Right conf' -> do
              let c = combineWithCmdLineArgs conf' mr
              print c
              case _binocularsProjectionPtype c of
                 QxQyQzProjection -> do
                   i <- mkInputQxQyQz c h5dpathQxQyQz
                   print i
                   processQxQyQz i
                 HklProjection -> do
                   i <- mkInputHkl c h5dpathHkl
                   print i
                   processHkl i
    Left e   -> print e
