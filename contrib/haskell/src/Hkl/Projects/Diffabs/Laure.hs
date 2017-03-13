{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Projects.Diffabs.Laure
       ( laure ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Array.Repa (DIM1, ix1)
import Data.Char (toUpper)
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (lookup, readFile, writeFile)

import Hkl

-- | TODO
-- * Livre 45 p159
-- * remove the air to each spectrum.
-- * deal with the multi geometry intensity problem. I = 1e9 ???
-- * simplify with the list of nxs using list comprehension.
-- * add the flyscan mesh
-- * add possibility to sum a bunch of pixel coordinates from a mesh. on a mask

-- | Samples

project ∷ FilePath
project = "/nfs/ruche-diffabs/diffabs-users/20160370/"

published ∷ FilePath
published = project </> "published-data" </> "xrd"
beamlineUpper ∷ Beamline → String
beamlineUpper b = [Data.Char.toUpper x | x ← show b]


-- | Calibration part

project' ∷ FilePath
project' = "/nfs/ruche-diffabs/diffabs-users/20160370/"

published' ∷ FilePath
published' = project' </> "published-data"

h5path' ∷ NxEntry → DataFrameH5Path XrdOneD
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

sampleRef ∷ XRDRef
sampleRef = XRDRef "reference"
            (published' </> "calibration")
            (XrdRefNxs
             (mkNxs (published' </> "calibration" </> "scan_45.nxs") "scan_44" h5path')
             10 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.
            )

sampleCalibration ∷ XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published' </> "calibration"
                                   , xrdCalibrationEntries = entries
                                   }
    where
      idxs ∷ [Int]
      idxs = [00, 01, 02, 03, 04, 09, 10, 11, 12, 14, 15, 18, 19, 22, 23, 26, 29, 33, 38, 42, 49, 53]

      entry ∷ Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryNxs
                { xrdCalibrationEntryNxs'Nxs = mkNxs (published' </> "calibration" </> "scan_45.nxs") "scan_44" h5path'
                , xrdCalibrationEntryNxs'Idx = idx
                , xrdCalibrationEntryNxs'NptPath = published' </> "calibration" </> printf "scan_45.nxs_%02d.npt" idx
                }

      entries ∷ [XRDCalibrationEntry]
      entries = [ entry idx | idx ← idxs]

-- | Data treatment

bins ∷ DIM1
bins = ix1 3000

multibins ∷ DIM1
multibins = ix1 25000

threshold ∷ Threshold
threshold = Threshold 800

skipedFrames ∷ [Int]
skipedFrames = [4]

-- Flat

mkNxs' ∷ FilePath → Int → (NxEntry → DataFrameH5Path a ) → Nxs a
mkNxs' d idx h = mkNxs f' e h
  where
     f ∷ FilePath → Int → (FilePath, NxEntry)
     f d' i' = (d' </> printf "scan_%d.nxs" i', printf "scan_%d" (i' - 1))

     (f', e) = f d idx

flat ∷ [Nxs XrdFlat]
flat = [mkNxs' (project </> "2017" </> "Run1" </> "2017-02-15") idx h5path | idx ← [57, 60 ∷ Int]] -- skip 58 59 for now (problème de droits d'accès)
  where
    h5path :: NxEntry -> DataFrameH5Path XrdFlat
    h5path nxentry = XrdFlatH5Path (DataItemH5 (nxentry </> "scan_data/data_02") StrictDims)

-- Scan en delta

h5path'' ∷ NxEntry -> DataFrameH5Path XrdOneD
h5path'' nxentry =
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

mkXRDSample ∷ String → [(FilePath, [Int])] -> XRDSample
mkXRDSample n ps = XRDSample n
                (published </> n)
                [ XrdNxs bins multibins threshold skipedFrames n' | n' ← concatMap nxs''' ps ]
    where
      nxs''' ∷ (FilePath, [Int]) → [XrdSource]
      nxs''' (p, idxs) = [XrdSourceNxs (mkNxs' p idx h5path'') | idx ← idxs]


air ∷ XRDSample
air = mkXRDSample "air" [ ((project </> "2017" </> "Run1" </> "2017-02-17"), [198 :: Int]) ]

samples :: [XRDSample]
samples = air : map (uncurry mkXRDSample)
          [ ("CeO2",               [ ((project </> "2017" </> "Run1" </> "2017-02-15"), [45 :: Int])  ])
          , ("kapton",             [ ((project </> "2017" </> "Run1" </> "2017-02-17"), [197 :: Int]) ])
          , ("chlorite",           [ ((project </> "2017" </> "Run1" </> "2017-02-15"), [53 :: Int])  ])
          , ("dMnO2",              [ ((project </> "2017" </> "Run1" </> "2017-02-16"), [135 :: Int]) ])
          , ("bulk_L2",            [ ((project </> "2017" </> "Shutdown1-2" </> "2017-02-19"), [315..316 :: Int]) ])
          , ("L1-H_3",             [ ((project </> "2017" </> "Run1" </> "2017-02-15"), concat [ [62..63 :: Int]
                                                                                               , [65..70 :: Int]
                                                                                               , [74, 75 :: Int]
                                                                                               ])
                                   , ((project </> "2017" </> "Run1" </> "2017-02-16"), [76..89 :: Int])
                                   ])
          , ("L1-H_4",             [ ((project </> "2017" </> "Run1" </> "2017-02-15"), [71..73 :: Int])
                                   , ((project </> "2017" </> "Run1" </> "2017-02-16"), concat [ [90..94 :: Int]
                                                                                               , [96..103 :: Int]
                                                                                               , [119..127 :: Int]
                                                                                               ])
                                   ])
          , ("L1-H_5",             [ ((project </> "2017" </> "Run1" </> "2017-02-16"), [104..118 :: Int]) ])
          , ("L1-Patine_1",        [ ((project </> "2017" </> "Run1" </> "2017-02-16"), [136..151 :: Int])
                                   , ((project </> "2017" </> "Run1" </> "2017-02-17"), concat [ [152..184 :: Int]
                                                                                               , [186 :: Int]
                                                                                               ])
                                   ])
          , ("L1-Patine_2",        [ ((project </> "2017" </> "Run1" </> "2017-02-17"), [187..196 :: Int]) ])
          , ("L2-H_1",             [ ((project </> "2017" </> "Run1" </> "2017-02-17"), [199..213 :: Int]) ])
          , ("L2-H_2",             [ ((project </> "2017" </> "Run1" </> "2017-02-17"), [214..220 :: Int])
                                   , ((project </> "2017" </> "Run1" </> "2017-02-18"), concat [ [221..228 :: Int]
                                                                                               , [259..262 :: Int]
                                                                                               ])
                                   ])
          , ("L2-H_3",             [ ((project </> "2017" </> "Run1" </> "2017-02-18"), [229..248 :: Int]) ])
          , ("L2-PatineFoncee",    [ ((project </> "2017" </> "Run1" </> "2017-02-18"), [249..258 :: Int]) ])
          , ("L2-PatineFonceeNew", [ ((project </> "2017" </> "Run1" </> "2017-02-18"), concat [ [263, 264, 266, 267 :: Int]
                                                                                               , [269..273 :: Int]])
                                   ])
          , ("L2-patineLabo_1",    [ ((project </> "2017" </> "Shutdown1-2" </> "2017-02-19"),[295..313 :: Int])  ])
          , ("L2-PatineClaire_1",  [ ((project </> "2017" </> "Shutdown1-2" </> "2017-02-19"), [317..324 :: Int])
                                   , ((project </> "2017" </> "Shutdown1-2" </> "2017-02-20"), [325..356 :: Int])
                                   ])
          , ("L3-patine_1",        [ ((project </> "2017" </> "Run1" </> "2017-02-19"), [274..293 :: Int])
                                   , ((project </> "2017" </> "Shutdown1-2" </> "2017-02-19"), [294, 295 :: Int])
                                   ])
          ]

-- | Main

laure ∷ IO ()
laure = do

  -- | compute the flat
  flat' ← computeFlat flat (published </> "flat" </> "flat.npy")

  -- | get a first ref poniExt
  p ← getPoniExtRef sampleRef
  -- flip the ref poni in order to fit the reality
  -- let poniextref = p
  let poniextref = setPose p (MyMatrix HklB (ident 3))
  -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))
  print poniextref

  -- | full calibration
  poniextref' ← calibrate sampleCalibration poniextref ImXpadS140
  print poniextref'

  -- | set the integration parameters
  let mflat = Just flat'
  let aiMethod = Csr
  let params = XrdOneDParams poniextref' mflat aiMethod

  -- integrate scan with multi geometry
  -- splitPixel (the only available now) → 17m47.825s
  _ ← mapM_ (integrateMulti params) samples

  -- Integrate each image of the scans
  -- Lut → 21.52 minutes
  -- Csr → 21.9 minutes
  _ ← mapConcurrently (integrate params) samples

  -- substrack the air from all samples
  substract params air samples
  substractMulti params air samples

  return ()
