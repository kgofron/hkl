{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Projects.Mars.Schlegel
       ( schlegel ) where

import System.FilePath ((</>))

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl

-- | TODO
-- * check if the
-- * find a way to use integrateMulti with a small amount of memory.
-- * better mask for each detector.

-- | Samples

project :: FilePath
project = "/nfs/share-temp/picca/20160800"

published :: FilePath
published = project </> "published-data"

h5path :: NxEntry -> DataFrameH5Path XrdZeroD
h5path nxentry =
  XrdZeroDH5Path
  (DataItemH5 (nxentry </> image) StrictDims)
  (DataItemConst 0.0485945)
  where
    image ∷ H5Path
    image = "scan_data/data_01"

sampleCalibration ∷ XrdZeroDCalibration Xpad32
sampleCalibration = XrdZeroDCalibration (XrdZeroDSample name outputdir entries) Xpad32 LaB6
    where
      name ∷ String
      name = "lab6"

      outputdir ∷ AbsDirPath
      outputdir = published </> "xrd" </> "calibration"

      entries :: [XrdZeroDSource]
      entries = [ XrdZeroDSourceNxs $
                  mkNxs (project </> "2017" </> "Run3" </> "scan_5_01.nxs") "_5" h5path
                ]


-- bins :: DIM1
-- bins = ix1 1000

-- multibins :: DIM1
-- multibins = ix1 10000

-- threshold :: Maybe Threshold
-- threshold = Just (Threshold 5000)

-- skipedFrames :: [Int]
-- skipedFrames = []

-- ceo2 :: XRDSample
-- ceo2 = XRDSample "CeO2"
--        (published </> "xrd" </> "CeO2")
--        [ XrdNxs bins multibins threshold skipedFrames (XrdSourceNxs n) | n <-
--          [ mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_29.nxs") "scan_29" h5path'
--          , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_30.nxs") "scan_30" h5path'
--          , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_56.nxs") "scan_56" h5path'
--          , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_58.nxs") "scan_58" h5path'
--          ]
--        ]

-- | Main

schlegel :: IO ()
schlegel = do
  -- | pre-calibrate (extract from nexus to edf in order to do the
  -- calibration)
  extractEdf sampleCalibration

  -- p <- getPoniExtRef sampleRef

  -- let poniextref = move p (Pose (MyMatrix HklB (ident 3)))

  -- -- full calibration
  -- poniextref' <- calibrate sampleCalibration poniextref

  -- print poniextref
  -- print poniextref'

  -- -- Integrate the flyscan mesh
  -- -- 4.680504680504681e-3 per images (2*60+18) / 29484 this contain
  -- -- read/write and computation
  -- -- integrateMesh (XrdMeshParams poniextref' mflat method) [fly]

  -- -- | set the integration parameters
  -- let mflat = Nothing
  -- let aiMethod = Csr
  -- let params = XrdOneDParams poniextref' mflat aiMethod

  -- -- integrate each step of the scan
  -- integrate params [ceo2]

  -- -- this code doesn not work because there is not enought memory on
  -- -- the computer.
  -- -- integrateMulti params [ceo2]

  return ()
