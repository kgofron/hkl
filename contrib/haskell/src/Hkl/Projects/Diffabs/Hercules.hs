{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Projects.Diffabs.Hercules
       ( hercules ) where

import Data.Array.Repa (DIM1, ix1)
import Numeric.Units.Dimensional.Prelude ((*~), centi, degree, meter)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (lookup, readFile, writeFile)

import Hkl

-- | Samples

project ∷ FilePath
project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

published ∷ FilePath
published = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/2017/Run2B/TPHercules"

-- | Calibration part

mkNxs' ∷ FilePath → Int → (NxEntry → DataFrameH5Path a ) → Nxs a
mkNxs' d idx h = mkNxs f' e h
  where
     f ∷ FilePath → Int → (FilePath, NxEntry)
     f d' i' = (d' </> printf "scan_%d.nxs" i', printf "scan_%d" i')

     (f', e) = f d idx

h5path ∷ NxEntry → DataFrameH5Path XrdOneD
h5path nxentry =
    XrdOneDH5Path
    (DataItemH5 (nxentry </> image) StrictDims)
    (DataItemH5 (nxentry </> beamline </> gamma) ExtendDims)
    (DataItemH5 (nxentry </> delta) ExtendDims)
    (DataItemH5 (nxentry </> beamline </> wavelength) StrictDims)
        where
          beamline :: String
          beamline = beamlineUpper Diffabs

          image = "scan_data/data_03"
          gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "scan_data/actuator_1_1"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

sampleRef ∷ XRDRef
sampleRef = XRDRef "reference"
            (published </> "calibration")
            (XrdRefNxs
             (mkNxs' (project </> "2017" </> "Run2" </> "2017-03-21") 91 h5path)
             15 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.
            )

sampleCalibration ∷ XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration"
                                   , xrdCalibrationEntries = entries
                                   }
    where
      idxs ∷ [Int]
      idxs = [15, 16, 17, 18, 19]

      entry ∷ Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryNxs
                { xrdCalibrationEntryNxs'Nxs = mkNxs' (project </> "2017" </> "Run2" </>  "2017-03-21") 91 h5path
                , xrdCalibrationEntryNxs'Idx = idx
                , xrdCalibrationEntryNxs'NptPath = published </> "calibration" </> printf "scan_91.nxs_%02d.npt" idx
                }

      entries ∷ [XRDCalibrationEntry]
      entries = map entry idxs

-- | Data treatment

bins ∷ DIM1
bins = ix1 3000

multibins ∷ DIM1
multibins = ix1 25000

threshold ∷ Maybe Threshold
threshold = Just (Threshold 800)

skipedFrames ∷ [Int]
skipedFrames = []

-- Flat

-- flat ∷ [Nxs XrdFlat]
-- flat = [mkNxs' (project </> "2017" </> "Run1" </> "2017-02-15") idx h5path | idx ← [57, 60 ∷ Int]] -- skip 58 59 for now (problème de droits d'accès)
--   where
--     h5path :: NxEntry -> DataFrameH5Path XrdFlat
--     h5path nxentry = XrdFlatH5Path (DataItemH5 (nxentry </> "scan_data/data_02") StrictDims)

-- Scan en delta

mkXRDSample ∷ String → [(FilePath, [Int])] -> XRDSample
mkXRDSample n ps = XRDSample n
                (published </> "xrd" </> n)
                [ XrdNxs bins multibins threshold skipedFrames n' | n' ← concatMap nxs''' ps ]
    where
      nxs''' ∷ (FilePath, [Int]) → [XrdSource]
      nxs''' (p, idxs) = [XrdSourceNxs (mkNxs' p idx h5path) | idx ← idxs]


samples :: [XRDSample]
samples = map (uncurry mkXRDSample)
          [ ("CeO2",            [ ((project </> "2017" </> "Run2" </> "2017-03-21"), [91 :: Int])  ])
          , ("zgso4_room",      [ ((project </> "2017" </> "Run2" </> "2017-03-21"), [96 :: Int]) ])
          , ("zgso4_450C",      [ ((project </> "2017" </> "Run2" </> "2017-03-21"), [192 :: Int]) ])
          , ("zgso4_heating",   [ ((project </> "2017" </> "Run2" </> "2017-03-21"), [100..190 :: Int]) ])
          , ("zgso4_cooling",   [ ((project </> "2017" </> "Run2" </> "2017-03-21"), [199..214 :: Int]) ])
          ]

-- | Main

hercules ∷ IO ()
hercules = do

  -- | pre-calibrate (extract from nexus to edf in order to do the
  -- calibration)
  extractEdf sampleCalibration

  -- | compute the flat
  -- flat' ← computeFlat flat (published </> "flat" </> "flat.npy")

  -- | get a first ref poniExt
  p ← getPoniExtRef sampleRef
  -- set the initial position of the poni (pyFAI calibration is not
  -- accurate with only one ring)
  let poniextref = set p
                   (63 *~ centi meter) -- distance
                   (0 *~ meter) -- poni1
                   (0 *~ meter) -- poni2
                   (0 *~ degree) -- rot1
                   (0 *~ degree) -- rot2
                   (0 *~ degree) -- rot3
  print poniextref

  -- | full calibration
  poniextref' ← calibrate sampleCalibration poniextref ImXpadS140
  print poniextref'

  -- | set the integration parameters
  let mflat = Nothing
  let aiMethod = Csr
  let params = XrdOneDParams poniextref' mflat aiMethod

  -- -- integrate scan with multi geometry
  -- -- splitPixel (the only available now) → 17m47.825s
  integrateMulti params samples

  -- -- Integrate each image of the scans
  -- -- Lut → 21.52 minutes
  -- -- Csr → 21.9 minutes
  -- integrate params samples

  -- -- substrack the air from all samples
  -- substract params air samples
  -- substractMulti params air samples

  return ()
