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

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.H5                     hiding (File)

h5dpathQxQyQz :: BinocularsConfig -> Maybe QxQyQzPath
h5dpathQxQyQz c = Just $ case _binocularsInputItype c of
  SixsFlyMedV -> QxQyQzPath
                  (DetectorPath
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                  (GeometryPathMedV
                   (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                    [ -- hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta" it was not saved in the file
                      hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa"
                    ])
  SixsFlyScanUhv -> QxQyQzPath
                   (DetectorPath
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                   (GeometryPathUhv
                    (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                    [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA"
                    ])
  SixsFlyScanUhv2 -> QxQyQzPath
                    (DetectorPath
                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                    (GeometryPathUhv
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                     [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                     ])
  SixsSbsMedV -> QxQyQzPath
                (DetectorPath
                 (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                (GeometryPathUhv
                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                 [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                 ])
  CristalK6C -> QxQyQzPath
               (DetectorPath
                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_05")) -- medipix
               (GeometryPathCristalK6C
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Monochromator" $ datasetp "lambda")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-mu" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-komega" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-kappa" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "actuator_1_1")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-gamma" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-delta" $ datasetp "position"))


--  FramesHklP

h5dpathHkl :: BinocularsConfig -> Maybe HklPath
h5dpathHkl c =
    let uhvSamplePath = SamplePath
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "A")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "B")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "C")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "alpha")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "beta")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "gamma")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "Ux")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "Uy")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "Uz")
        cmMedVSamplePath = SamplePath
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "A")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "B")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "C")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "alpha")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "beta")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "gamma")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "Ux")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "Uy")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-cm-med.v" $ datasetp "Uz")
    in case h5dpathQxQyQz c of
         (Just qxqyqz) -> case _binocularsInputItype c of
                           SixsFlyMedV -> Just (HklPath qxqyqz cmMedVSamplePath)
                           SixsFlyScanUhv -> Just (HklPath qxqyqz uhvSamplePath)
                           SixsFlyScanUhv2 -> Just (HklPath qxqyqz uhvSamplePath)
                           SixsSbsMedV -> undefined
                           CristalK6C -> fmap (HklPathFromQxQyQz qxqyqz) (sampleConfig c)
         Nothing -> Nothing

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
