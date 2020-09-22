{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Projects.Diffabs.IRDRx
       ( irdrx ) where

import           Data.Array.Repa       (DIM1, ix1)
import           Numeric.LinearAlgebra (ident)
import           System.FilePath       ((</>))
import           Text.Printf           (printf)

import           Prelude               hiding (concat, lookup, readFile,
                                        writeFile)

import           Hkl

--  Samples

project :: FilePath
project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

published :: FilePath
published = project </> "2016" </> "Run5B" </> "irdrx"

sampleRef :: XRDRef
sampleRef = XRDRef "reference"
            (published </> "calibration")
            (XrdRefNxs
             (mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_39.nxs") "scan_39" h5path')
             10
            )

h5path' :: NxEntry -> DataFrameH5Path XrdOneD
h5path' nxentry =
    XrdOneDH5Path
    (DataItemH5 (nxentry </> image) StrictDims)
    (DataItemH5 (nxentry </> beamline </> gamma) ExtendDims)
    (DataItemH5 (nxentry </> delta) ExtendDims)
    (DataItemH5 (nxentry </> beamline </> wavelength) StrictDims)
        where
          beamline :: String
          beamline = beamlineUpper Diffabs

          image = "scan_data/data_05"
          gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "scan_data/data_03"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

sampleCalibration :: XRDCalibration PyFAI
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration" -- TODO pourquoi ce output
                                   , xrdCalibrationDetector = ImXpadS140
                                   , xrdCalibrationCalibrant = CeO2
                                   , xrdCalibrationEntries = entries
                                   }
    where

      idxs :: [Int]
      idxs = [0, 1, 10, 30]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryNxs
                { xrdCalibrationEntryNxs'Nxs = mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_39.nxs") "scan_39" h5path'
                , xrdCalibrationEntryNxs'Idx = idx
                , xrdCalibrationEntryNxs'NptPath = published </> "calibration" </> printf "scan_39.nxs_%02d.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]


bins :: DIM1
bins = ix1 1000

multibins :: DIM1
multibins = ix1 10000

threshold :: Maybe Threshold
threshold = Just (Threshold 5000)

skipedFrames :: [Int]
skipedFrames = []

lab6 :: XRDSample
lab6 = XRDSample "LaB6"
       (published </> "LaB6")
       [ XrdNxs bins multibins threshold skipedFrames (XrdSourceNxs n) | n <-
         [ mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_39.nxs") "scan_39" h5path'
         , mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_40.nxs") "scan_40" h5path'
         , mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_41.nxs") "scan_41" h5path'
         , mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_42.nxs") "scan_42" h5path'
         , mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_43.nxs") "scan_43" h5path'
         , mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_44.nxs") "scan_44" h5path'
         , mkNxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_45.nxs") "scan_45" h5path'
         ]
       ]



-- meshSample :: String
-- meshSample :: project </> "2016" </> Run5 </> "2016-11-fly" </> "scan5 </> "*"
-- h5path nxentry = exptest_01368
-- scan_data, sxpos szpos xpad_image 12x273 x 10 (fichiers)
-- delta = -6.2
-- gamma = 0.0
-- nrj 18.2 keV
fly :: XrdMeshSample
fly = XrdMeshSample "scan5"
      (published </> "scan5")
      [ XrdMesh bins multibins threshold
        ( XrdMeshSourceNxsFly [mkNxs (project </> "2016" </> "Run5" </> "2016-11-fly" </> "scan5" </> printf "flyscan_%05d.nxs" n) "exptest_01368" h5path |
                               n <- [7087, 7088, 7089, 7090, 7091, 7092, 7093, 7094, 7095] :: [Int]
                              ]
        )
      ]
    where
      h5path :: NxEntry -> DataFrameH5Path XrdMesh
      h5path nxentry = XrdMeshFlyH5Path
                       (DataItemH5 (nxentry </> image) StrictDims)
                       (DataItemH5 (nxentry </> meshx) StrictDims)
                       (DataItemH5 (nxentry </> meshy) StrictDims)
                       (DataItemConst gamma)
                       (DataItemConst delta)
                       (DataItemConst wavelength)

      beamline :: String
      beamline = beamlineUpper Diffabs

      image = "scan_data/xpad_image"
      meshx = "scan_data/sxpos"
      meshy = "scan_data/szpos"
      gamma = 0.0 / 180.0 * 3.14159
      delta = -6.2 / 180.0 * 3.14159
      wavelength = 1.54 -- TODO vérifier

--  Main

irdrx :: IO ()
irdrx = do
  let mflat = Nothing
  let method = CsrOcl

  p <- getPoniExtRef sampleRef

  let poniextref = move (Hkl.flip p) (Pose (MyMatrix HklB (ident 3)))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref

  print poniextref'

  -- Integrate the flyscan mesh
  -- 4.680504680504681e-3 per images (2*60+18) / 29484 this contain
  -- read/write and computation
  integrateMesh (XrdMeshParams poniextref' mflat method) [fly]

  -- integrate each step of the scan
  -- _ <- mapConcurrently (integrate poniextref') [lab6]
  return ()
