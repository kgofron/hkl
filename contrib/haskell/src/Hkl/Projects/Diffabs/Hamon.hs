{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Projects.Diffabs.Hamon
       ( hamon ) where

import Data.Array.Repa (DIM1, ix1)
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl

-- | TODO
-- * take into account a non-centered sample.
-- * better script for pyFAI-calibration (take the wavelength from the nexus file, make it executable by default)
-- * find a way to use integrateMulti with a small amount of memory.
-- * better mask for each detector.

-- | Samples

project :: FilePath
project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

published :: FilePath
published = project </> "2016" </> "Run4B" </> "OutilsMetallo_CarolineHamon"

sampleRef :: XRDRef
sampleRef = XRDRef "reference"
            (published </> "xrd" </> "calibration")
            (XrdRefNxs
             (mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_30.nxs") "scan_30" h5path')
             33
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

          image = "scan_data/data_02"
          gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "scan_data/actuator_1_1"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

sampleCalibration :: XRDCalibration Xpad32
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "xrd" </> "calibration" -- TODO pourquoi ce output
                                   , xrdCalibrationDetector = Xpad32
                                   , xrdCalibrationCalibrant = CeO2
                                   , xrdCalibrationEntries = entries
                                   }
    where

      idxs :: [Int]
      idxs = [5, 33, 100, 246, 300, 436]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryNxs
                { xrdCalibrationEntryNxs'Nxs = mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_30.nxs") "scan_30" h5path'
                , xrdCalibrationEntryNxs'Idx = idx
                , xrdCalibrationEntryNxs'NptPath = published </> "xrd" </> "calibration" </> printf "IHR_30.nxs_%02d.npt" idx
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

ceo2 :: XRDSample
ceo2 = XRDSample "CeO2"
       (published </> "xrd" </> "CeO2")
       [ XrdNxs bins multibins threshold skipedFrames (XrdSourceNxs n) | n <-
         [ mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_29.nxs") "scan_29" h5path'
         , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_30.nxs") "scan_30" h5path'
         , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_56.nxs") "scan_56" h5path'
         , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_58.nxs") "scan_58" h5path'
         ]
       ]

-- | Main

hamon :: IO ()
hamon = do
  -- | pre-calibrate (extract from nexus to edf in order to do the
  -- calibration)
  extractEdf sampleCalibration

  p <- getPoniExtRef sampleRef

  let poniextref = move p (MyMatrix HklB (ident 3))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref

  print poniextref
  print poniextref'

  -- Integrate the flyscan mesh
  -- 4.680504680504681e-3 per images (2*60+18) / 29484 this contain
  -- read/write and computation
  -- integrateMesh (XrdMeshParams poniextref' mflat method) [fly]

  -- | set the integration parameters
  let mflat = Nothing
  let aiMethod = Csr
  let params = XrdOneDParams poniextref mflat aiMethod

  -- integrate each step of the scan
  integrate params [ceo2]

  -- this code doesn not work because there is not enought memory on
  -- the computer.
  -- integrateMulti params [ceo2]

  return ()
