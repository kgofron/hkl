{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Projects.Diffabs.Charlier
       ( charlier ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Array.Repa (DIM1, ix1)
import Data.Char (toUpper)
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl

-- | Samples

project :: FilePath
project = "/nfs/ruche-diffabs/diffabs-users/20151386/"

published :: FilePath
published = project </> "published-data" </> "xrd"

beamlineUpper :: Beamline -> String
beamlineUpper b = [Data.Char.toUpper x | x <- show b]


-- | Calibration part

project' :: FilePath
project' = "/nfs/ruche-diffabs/diffabs-users/99160066/"

published':: FilePath
published' = project' </> "published-data"

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

          image = "scan_data/data_53"
          gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "scan_data/actuator_1_1"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

sampleCalibration :: XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published' </> "calibration"
                                   , xrdCalibrationEntries = entries
                                   }
    where
      idxs :: [Int]
      idxs = [3, 6, 9, 15, 18, 21, 24, 27, 30, 33, 36, 39, 43]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryNxs
                { xrdCalibrationEntryNxs'Nxs = mkNxs (published' </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path'
                , xrdCalibrationEntryNxs'Idx = idx
                , xrdCalibrationEntryNxs'NptPath = published' </> "calibration" </> printf "XRD18keV_26.nxs_%02d.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]


sampleRef :: XRDRef
sampleRef = XRDRef "reference"
            (published' </> "calibration")
            (XrdRefNxs
             (mkNxs (published' </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path')
             6 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.
            )

bins :: DIM1
bins = ix1 8000

multibins :: DIM1
multibins = ix1 25000

threshold :: Threshold
threshold = Threshold 800

h5path :: NxEntry -> DataFrameH5Path XrdMesh
h5path nxentry = XrdMeshH5Path
                 (DataItemH5 (nxentry </> image) StrictDims)
                 (DataItemH5 (nxentry </> meshx) StrictDims)
                 (DataItemH5 (nxentry </> meshy) StrictDims)
                 (DataItemH5 (nxentry </> beamline </> gamma) StrictDims)
                 (DataItemH5 (nxentry </> beamline </> delta) StrictDims)
                 (DataItemH5 (nxentry </> beamline </> wavelength) StrictDims)
        where
          beamline :: String
          beamline = beamlineUpper Diffabs

          image = "scan_data/data_53"
          meshx = "scan_data/actuator_1_1"
          meshy = "scan_data/actuator_2_1"
          gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "d13-1-cx1__EX__DIF.1-DELTA__#1/raw_value"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

charlemagne :: XrdMeshSample
charlemagne = XrdMeshSample "Charlemagne"
       (published </> "Charlemagne")
       [ XrdMesh bins multibins threshold (XrdMeshSourceNxs n) | n <-
         [ mkNxs (project </> "2016" </> "Run2" </> "2016-03-23" </> "XRD18keV_31.nxs") "scan_31" h5path
         , mkNxs (project </> "2016" </> "Run2" </> "2016-03-23" </> "XRD18keV_32.nxs") "scan_32" h5path
         , mkNxs (project </> "2016" </> "Run2" </> "2016-03-23" </> "XRD18keV_33.nxs") "scan_33" h5path
         ]
       ]

charlesLeChauve :: XrdMeshSample
charlesLeChauve = XrdMeshSample "Charles le Chauve"
       (published </> "Charles le Chauve")
       [ XrdMesh bins multibins threshold (XrdMeshSourceNxs n) | n <-
         [ mkNxs (project </> "2016" </> "Run2" </> "2016-03-24" </> "XRD18keV_34.nxs") "scan_34" h5path ]
       ]

louisLePieux :: XrdMeshSample
louisLePieux = XrdMeshSample "Louis le Pieux"
       (published </> "Louis Le Pieux")
       [ XrdMesh bins multibins threshold (XrdMeshSourceNxs n) | n <-
         [ mkNxs (project </> "2016" </> "Run2" </> "2016-03-24" </> "XRD18keV_35.nxs") "scan_35" h5path
         , mkNxs (project </> "2016" </> "Run2" </> "2016-03-24" </> "XRD18keV_36.nxs") "scan_36" h5path
         , mkNxs (project </> "2016" </> "Run2" </> "2016-03-24" </> "XRD18keV_37.nxs") "scan_37" h5path
         ]
       ]

-- | Main

charlier :: IO ()
charlier = do
  let samples = [ charlemagne, charlesLeChauve, louisLePieux]
  -- # need to run f30 by itself because of a segfault in the hkl library
  -- for now f30 whcih is an incomplet scan stop the script so put it at the end.
  -- let samples = [f30, ceo2]
  -- let samples = [ceo2]
  let mflat = Nothing
  let method = CsrOcl

  p <- getPoniExtRef sampleRef

  -- flip the ref poni in order to fit the reality
  -- let poniextref = p
  let poniextref = setPose p (MyMatrix HklB (ident 3))
  -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref Xpad32
  -- print p
  print poniextref
  print poniextref'

  -- integrate each step of the scan
  _ <- mapConcurrently (integrateMesh (XrdMeshParams poniextref' mflat method)) samples
  return ()
