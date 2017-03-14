{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Projects.D2AM.XRD
       ( d2am ) where

import Data.Array.Repa (DIM1, ix1)
-- import Data.Char (toUpper)
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl.MyMatrix
import Hkl.PyFAI
import Hkl.Xrd
import Hkl.Detector

-- | Samples

project :: FilePath
project = "/home/experiences/instrumentation/picca/data/d2am"
-- project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

published :: FilePath
published = project </> "published-data"

sampleRef :: XRDRef
sampleRef =  XRDRef "reference"
            (published </> "calibration")
            (XrdRefEdf
             (project </> "16Dec08D5_0268-rsz.edf")
             (project </> "16Dec08D5_0268-rsz.poni")
            )

sampleCalibration :: XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration" -- TODO pourquoi ce output
                                   , xrdCalibrationEntries = entries
                                   }
    where

      idxs :: [Int]
      idxs = [268, 271, 285, 295]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryEdf
                { xrdCalibrationEntryEdf'Edf = project </> printf "16Dec08D5_%04d-rsz.edf" idx
                , xrdCalibrationEntryEdf'NptPath = project </> printf "16Dec08D5_%04d-rsz.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]

bins :: DIM1
bins = ix1 1000

multibins :: DIM1
multibins = ix1 10000

threshold :: Threshold
threshold = Threshold 5000

skipedFrames :: [Int]
skipedFrames = []

lab6 :: XRDSample
lab6 = XRDSample "test"
       (published </> "test")
       [XrdNxs bins multibins threshold skipedFrames entries]
           where
             idxs :: [Int]
             idxs = [268, 271, 285, 295]

             entry :: Int -> FilePath
             entry idx = project </> printf "16Dec08D5_%04d-rsz.edf" idx

             entries :: XrdSource
             entries = XrdSourceEdf [entry idx | idx <- idxs]

-- | Main

d2am :: IO ()
d2am = do
  let samples = [lab6]

  p <- getPoniExtRef sampleRef

  -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))
  let poniextref = setPose p (MyMatrix HklB (ident 3))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref Xpad32

  print poniextref
  print poniextref'

  -- integrate each step of the scan
  let params = XrdOneDParams poniextref' Nothing Csr -- waiting for PyFAI to manage method in multi geometry
  integrateMulti params samples

  return ()
