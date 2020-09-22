{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Hkl.Projects.Diffabs.Melle
       ( melle ) where

-- import Control.Concurrent (setNumCapabilities)
-- import Control.Concurrent.Async (mapConcurrently)
import           Data.Array.Repa (DIM1, ix1)
-- import Data.Char (toUpper)
-- import Numeric.LinearAlgebra (ident)
import           System.FilePath ((</>))
import           Text.Printf     (printf)

-- import Prelude hiding (concat, lookup, readFile, writeFile)

import           Hkl

published ∷ FilePath
published = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/Reguer/USERSexperiences/melle"

--  TODO

--  MELLE / VIALAS
-- Session 1 MACRO - 16-17 février 2016 (Logbook n° 42 p 169)
-- Session 2 MICRO KB --28 mars 2016 (Logbook 42 + Logbook 43 p3)
-- Session 3 MICRO pinhole - 22-24 juillet 2016 (Logbook 44 p33)
-- Session 4 MACRO - septembre 2016 (Logbook 44 p63)

--  Session 1

-- macrofaisceau
-- 16keV
-- Λ = 0,775
-- detection : XPAD S140 / image = data 54
-- sample : ω = 5 et χ = 70

-- calibration = beam direct

-- - 3 MESH pour 3 positions du détecteur de diffraction (delta = -4, 3, 10),

-- macro python:
-- for i in range (10):
--     myx = -12+i*0,5
--     mv(samplex, myx)
--     ascan(sampley, -8, 12, 100, 10)

-- scan_26 à 55.nxs
-- diffabs-soleil/com-diffabs/2016/Run1/2016-02-16 ou 02-17

-- 2THETA = 1 DELTA SCAN
-- scan_56 = ascan(delta, -4, 70, 18, 3)
-- scan_58 = ascan(delta, -4, 70, 18, 3)


--  Session 2

-- microbeam
-- 18keV, ?= 0,6888Å
-- detection : XPAD 3.2 / image = data  58
-- sample : ? = 5° et ? = 80°.
-- calibration CeO2
-- data dans le dossier du proposal de Philippe Charlier 2015 1386
-- voir aussi script Martinetto  proposal IHR 99160066
-- scan_25  = ascan(delta, -14.5, 60.5, 75, 0.5)
-- scan_26 = ascan(delta, -14, 60, 75, 1)
-- scan_27 = ascan(delta, -14, 60, 46, 1)

-- MESH : MELLE_29.nxs
-- dossier: diffabs-soleil/com-diffabs/2016/Run2/2016-03-28

-- calibration

project2 :: FilePath
project2 = "/nfs/ruche-diffabs/diffabs-users/99160066/"

published2:: FilePath
published2 = project2 </> "published-data"

h5path2 :: NxEntry -> DataFrameH5Path XrdOneD
h5path2 nxentry =
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

sampleCalibration2 :: XRDCalibration PyFAI
sampleCalibration2 = XRDCalibration { xrdCalibrationName = "calibration2"
                                    , xrdCalibrationOutputDir = published </> "calibration2"
                                    , xrdCalibrationDetector = Xpad32
                                    , xrdCalibrationCalibrant = CeO2
                                    , xrdCalibrationEntries = entries
                                    }
    where
      idxs :: [Int]
      idxs = [3, 6, 9, 15, 18, 21, 24, 27, 30, 33, 36, 39, 43]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryNxs
                { xrdCalibrationEntryNxs'Nxs = mkNxs (published2 </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path2
                , xrdCalibrationEntryNxs'Idx = idx
                , xrdCalibrationEntryNxs'NptPath = published2 </> "calibration" </> printf "XRD18keV_26.nxs_%02d.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]

sampleRef2 :: XRDRef
sampleRef2 = XRDRef "reference"
             (published2 </> "calibration")
             (XrdRefNxs
              (mkNxs (published2 </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path2)
              6 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.
             )

bins :: DIM1
bins = ix1 8000

multibins :: DIM1
multibins = ix1 25000

threshold :: Maybe Threshold
threshold = Just (Threshold 800)

skipedFrames :: [Int]
skipedFrames = []

melleScan :: XRDSample
melleScan = XRDSample "CeO2"
       (published </> "xrd" </> "session2" </> "oned")
       [ XrdNxs bins multibins threshold skipedFrames (XrdSourceNxs n) | n <-
         [ mkNxs (project </> "2016" </> "Run2" </> "2016-03-23" </> "XRD18keV_25.nxs") "scan_25" h5path2
         , mkNxs (project </> "2016" </> "Run2" </> "2016-03-23" </> "XRD18keV_26.nxs") "scan_26" h5path2
         , mkNxs (project </> "2016" </> "Run2" </> "2016-03-23" </> "XRD18keV_27.nxs") "scan_27" h5path2
         ]
       ]
  where
    project ∷ FilePath
    project = "/nfs/ruche-diffabs/diffabs-users/20151386/"


melleMesh :: XrdMeshSample
melleMesh = XrdMeshSample "MELLE_29"
          (published </> "xrd" </> "session2" </> "mesh")
          [ XrdMesh bins multibins threshold (XrdMeshSourceNxs n) | n <-
            [ mkNxs (project2' </> "2016" </> "Run2" </> "2016-03-28" </> "MELLE_29.nxs") "scan_29" h5path2'
            ]
          ]
  where
    project2' :: FilePath
    project2' = "/nfs/ruche-diffabs/diffabs-users/99160066/"

    h5path2' :: NxEntry -> DataFrameH5Path XrdMesh
    h5path2' nxentry =
      XrdMeshH5Path
      (DataItemH5 (nxentry </> image) StrictDims)
      (DataItemH5 (nxentry </> meshX) StrictDims)
      (DataItemH5 (nxentry </> meshY) StrictDims)
      (DataItemH5 (nxentry </> beamline </> gamma) ExtendDims)
      (DataItemH5 (nxentry </> beamline </> delta) ExtendDims)
      (DataItemH5 (nxentry </> beamline </> wavelength) StrictDims)
      where
        beamline :: String
        beamline = beamlineUpper Diffabs

        image = "scan_data/data_58"
        meshX = "scan_data/actuator_1_1"
        meshY = "scan_data/actuator_2_1"
        gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
        delta = "D13-1-CX1__EX__DIF.1-DELTA__#1/raw_value"
        wavelength = "D13-1-C03__OP__MONO__#1/wavelength"


session2 :: IO ()
session2 = do
  -- compute the ref poni
  p ← getPoniExtRef sampleRef2
  poniextref <- calibrate sampleCalibration2 p

  -- integrate the mesh
  let mflat = Nothing
  integrateMesh (XrdMeshParams poniextref mflat CsrOcl) [melleMesh]

  -- integrate the scan parts
  let params = XrdOneDParams poniextref mflat Csr
  integrate params [melleScan]
  integrateMulti params [melleScan]

  return ()

--  session 4
-- macro
-- 18keV, ?= 0,6888Å
-- detection : XPAD 3.2

session4 ∷ IO ()
session4 = do
  -- calibration
  p ← getPoniExtRef sampleRef
  poniextref <- calibrate sampleCalibration p

-- calibration : CeO2
-- On peut utiliser la calib de IHR_30, mais il faut prendre en compte le décentrage.
-- IHR_56
-- IHR_58
-- sont deux autres possibilité de calibration.
-- diffabs-soleil\com-diffabs\2016\Run4\2016-09-07

  --  set the integration parameters
  let mflat = Nothing
  let params = XrdOneDParams poniextref mflat Csr

  -- integrate each step of the scan
  integrate params [ceo2]

-- 1 seul "MESH"(20, 49)  à partir d'une serie 2THETA
-- IHR_63 à 95
-- diffabs-soleil\com-diffabs\2016\Run4\2016-09-07
-- IHR_96 à 190
-- diffabs-soleil\com-diffabs\2016\Run4\2016-09-08
-- obtenu via la macro suivante.
-- for i in range(20):
--    myx = -11 + i
--    mv(txs, myx)  # exhantillon à 45 degree donc ce double déplacement correspond au vrai x
--    mv(tys, myx)
--    for j in range(29):
--         myy = 12 + j
--         mv(tabV, myy)
--         ascan(δ, -13.6, 30, 109, 5)

  return ()

  where

    project :: FilePath
    project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

    published' :: FilePath
    published' = project </> "2016" </> "Run4B" </> "OutilsMetallo_CarolineHamon"

    sampleRef :: XRDRef
    sampleRef = XRDRef "reference"
                (published' </> "xrd" </> "calibration")
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

    sampleCalibration :: XRDCalibration PyFAI
    sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                       , xrdCalibrationOutputDir = published' </> "xrd" </> "calibration" -- TODO pourquoi ce output
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
                , xrdCalibrationEntryNxs'NptPath = published' </> "xrd" </> "calibration" </> printf "IHR_30.nxs_%02d.npt" idx
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
           (published </> "session4" </> "xrd" </> "CeO2")
           [ XrdNxs bins multibins threshold skipedFrames (XrdSourceNxs n) | n <-
                [ mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_29.nxs") "scan_29" h5path'
                , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_30.nxs") "scan_30" h5path'
                , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_56.nxs") "scan_56" h5path'
                , mkNxs (project </> "2016" </> "Run4" </> "2016-09-07" </> "IHR_58.nxs") "scan_58" h5path'
                ]
           ]

-- * session 5
-- micro
-- 18.05keV
-- detection XPAD S140

-- calibration CeO2
-- gam = 9 phi = 170
-- 18p1kev_71
-- gam = 9 phi = 175
-- 18p1kev_73
-- gam = 0  phi = 205
-- 18p1kev_74
-- gam = 0.3  phi = 205
-- 18p1kev_75
-- ruche-diffabs\diffabs-users\99170085\2017\Run3\2017-05-14

-- FLAT (à verifier si suffisant) (faire la somme des trois fichiers)
-- 18p1kev_82
-- 18p1kev_83
-- 18p1kev_84
-- ruche-diffabs\diffabs-users\99170085\2017\Run3\2017-05-14

-- FLY -- ????
-- flyscan_16602
-- diffabs-soleil\com-diffabs\2017\Run3\fly_IHRSol

-- 2THETA = 1 DELTA SCAN
-- 18p1kev_85
-- 18p1kev_86
-- ruche-diffabs\diffabs-users\99170085\2017\Run3\2017-05-14

--  Samples

-- published :: FilePath
-- published = project </> "published-data"

-- beamlineUpper :: Beamline -> String
-- beamlineUpper b = [Data.Char.toUpper x | x <- show b]

-- nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path) -> Nxs
-- nxs f e h = Nxs f e (h e)

-- nxs' :: FilePath -> NxEntry -> (NxEntry -> a) -> Nxs' a
-- nxs' f e h = Nxs' f e (h e)

-- h5path :: NxEntry -> DataFrameH5Path
-- h5path nxentry =
--     DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
--                     , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
--                     , h5pDelta = DataItem (nxentry </> delta) ExtendDims
--                     , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
--                     }
--         where
--           beamline :: String
--           beamline = beamlineUpper Diffabs

--           image = "scan_data/data_53"
--           gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
--           delta = "scan_data/actuator_1_1"
--           wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

-- sampleCalibration :: XRDCalibration
-- sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
--                                    , xrdCalibrationOutputDir = published </> "calibration"
--                                    , xrdCalibrationEntries = entries
--                                    }
--     where

--       idxs :: [Int]
--       idxs = [3, 6, 9, 15, 18, 21, 24, 27, 30, 33, 36, 39, 43]

--       entry :: Int -> XRDCalibrationEntry
--       entry idx = XRDCalibrationEntryNxs
--                 { xrdCalibrationEntryNxs'Nxs = nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path
--                 , xrdCalibrationEntryNxs'Idx = idx
--                 , xrdCalibrationEntryNxs'NptPath = published </> "calibration" </> printf "XRD18keV_26.nxs_%02d.npt" idx
--                 }

--       entries :: [XRDCalibrationEntry]
--       entries = [ entry idx | idx <- idxs]


-- sampleRef :: XRDRef
-- sampleRef = XRDRef "reference"
--             (published </> "calibration")
--             (nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path)
--             6 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.

-- bins :: DIM1
-- bins = ix1 8000

-- multibins :: DIM1
-- multibins = ix1 25000

-- threshold :: Threshold
-- threshold = Threshold 800


--   p <- getPoniExtRef sampleRef

--   -- flip the ref poni in order to fit the reality
--   -- let poniextref = p
--   let poniextref = setPose p (MyMatrix HklB (ident 3))
--   -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))

--   -- full calibration
--   poniextref' <- calibrate sampleCalibration poniextref Xpad32
--   -- print p
--   print poniextref
--   print poniextref'

--   -- integrate each step of the scan
--   _ <- mapM_ (integrateMesh poniextref') samples

--   return ()

melle ∷ IO ()
melle = do
  session2
  session4
