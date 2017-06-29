{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Projects.Mars.Romeden
       ( romeden ) where

import Codec.Picture ( saveTiffImage )
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl

-- | TODO
-- first extract only the images for each nexus file
-- /nfs/ruche-mars/mars-soleil/com-mars/2017_Run2/commisioning_microfaisceau
-- Scan_101_01.nxs → LaB6
-- EM10_600C_1000h_profile_1_scan128_xx.nxs pour xx [1 → 13] en tiff
-- EM10_500C_5000h_profile_1_scan154_xx.nxs pour xx [1 → 12] en tiff
-- scan_172_01.nxs -> Cr Exptime 30s
-- scan_173_01.nxs -> Cr Exptime 60s
-- scan_174_01.nxs -> Cr Exptime 60s
-- scan_175_01.nxs -> Fe Exptime 60s
-- scan_176_01.nxs -> Fe Exptime 300s
-- scan_178_01.nxs -> Fe Exptime 30s
-- scan_179_01.nxs -> Fe Exptime 10s

-- * check if the
-- * find a way to use integrateMulti with a small amount of memory.
-- * better mask for each detector.

-- | Samples

project :: FilePath
-- project = "/nfs/ruche-mars/mars-soleil/com-mars/2017_Run2/comisioning_microfaisceau"
project = "/home/experiences/instrumentation/picca"
-- project = "/media/picca/Transcend/ROMEDENNE"

published :: FilePath
published = project </> "published-data"

h5path :: NxEntry -> DataFrameH5Path XrdFlat
h5path nxentry =
  XrdFlatH5Path
  (DataItemH5 (nxentry </> image) StrictDims)
  where
    image ∷ H5Path
    image = "image#0/data"

-- sampleCalibration ∷ XrdZeroDCalibration Xpad32
-- sampleCalibration = XrdZeroDCalibration (XrdZeroDSample name outputdir entries) Xpad32 LaB6
--     where
--       name ∷ String
--       name = "lab6"

--       outputdir ∷ AbsDirPath
--       outputdir = published </> "xrd" </> "calibration"

--       entries :: [XrdZeroDSource]
--       entries = [ XrdZeroDSourceNxs $
--                   -- mkNxs (project </> "2017" </> "Run3" </> "scan_5_01.nxs") "_5" h5path
--                   mkNxs (project </> "EM10_600C_1000h_profile_1scan_128_01.nxs") "_137" h5path
--                 ]


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
em10_500 ∷ [FilePath]
em10_500 = [ printf "EM10_500C_5000h_profile_1scan_154_%02d.nxs" i | i ← [1..12 ∷ Int] ]

em10_600 ∷ [FilePath]
em10_600 = [ printf "EM10_600C_1000h_profile_1scan_128_%02d.nxs" i | i ← [1..13 ∷ Int] ]

scans ∷ [FilePath]
scans = [ printf "scan_%d_01.nxs" i | i ← (101 : [171..180 ∷ Int])]

samples ∷ [FilePath]
samples = em10_500 ++ em10_600 ++ scans

sample ∷ FilePath
sample = project </> "EM10_600C_1000h_profile_1scan_128_01.nxs"


saveAsTiff' ∷ (Nxs XrdFlat, FilePath) → IO ()
saveAsTiff' (n, o) = saveTiffImage o =<< toTiff n

saveAsTiff ∷ FilePath → (NxEntry -> DataFrameH5Path XrdFlat) → IO ()
saveAsTiff n h5path' = do
  ns ← nxEntries n
  let nxs' = map nxs ns
  let outs = map out ns
  mapM_ saveAsTiff' (zip nxs' outs)
  where
    nxs ∷ FilePath → Nxs XrdFlat
    nxs nx = mkNxs (project </> n) nx h5path'

    out ∷ FilePath → FilePath
    out nx = (project </> n) ++  nx ++ ".tiff"

-- | Main

romeden :: IO ()
romeden = do
  -- | pre-calibrate (extract from nexus to edf in order to do the
  -- calibration)
  -- print samples
  saveAsTiff sample h5path
  -- mapM_ (\f → saveAsTiff f h5path) samples

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
