{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Xrd.OneD
       ( XrdOneD
       , XRDRef(..)
       , XrdRefSource(..)
       , XRDSample(..)
       , OutputBaseDir
       , SampleName
       , Threshold(..)
       , XrdNxs(..)
       , XrdOneDParams(..)
       , XrdSource(..)
       , PoniExt(..)
         -- reference
       , getMEdf
       , getPoniExtRef
         -- integration
       , integrate
       , substract
         -- integrateMulti
       , integrateMulti
       ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, forever, void, when, zipWithM_)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Array.Repa (DIM1, ix1, size)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.List as List (intercalate, lookup)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text (unlines, pack, intercalate)
import Data.Text.IO (readFile)
import Data.Vector.Storable (concat, head)
import Numeric.LinearAlgebra (fromList)
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ((<.>), (</>), dropExtension, replaceExtension, takeFileName, takeDirectory)
import Text.Printf ( printf )

import Prelude hiding
    ( any
    , concat
    , filter
    , head
    , lookup
    , readFile
    , unlines
    )
import Pipes
    ( Consumer
    , Pipe
    , lift
    , (>->)
    , runEffect
    , await
    , yield
    )
import Pipes.Lift
import Pipes.Prelude (drain, filter, toListM)
import Pipes.Safe (runSafeT)

import Hkl.C
import Hkl.DataSource
import Hkl.Detector
import Hkl.Edf
import Hkl.Flat
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.Nxs
import Hkl.Script
import Hkl.Types
import Hkl.Utils

-- | Types

type OutputBaseDir = FilePath
type SampleName = String

data Threshold = Threshold Int deriving (Show)


data XrdRefSource = XrdRefNxs (Nxs XrdOneD) Int
                  | XrdRefEdf FilePath FilePath
                  deriving (Show)

data XRDRef = XRDRef SampleName OutputBaseDir XrdRefSource
            deriving (Show)

data XrdSource = XrdSourceNxs (Nxs XrdOneD)
               | XrdSourceEdf [FilePath]
                 deriving (Show)

data XrdNxs
    = XrdNxs
      DIM1 -- bins
      DIM1 -- bins for the multibins
      Threshold -- threshold use to remove image Intensity
      [Int] -- Index of the frames to skip
      XrdSource -- data source
    deriving (Show)

data XRDSample = XRDSample SampleName OutputBaseDir [XrdNxs] -- ^ nxss
               deriving (Show)

data XrdOneDParams a = XrdOneDParams PoniExt (Maybe (Flat a)) AIMethod

data DifTomoFrame sh =
  DifTomoFrame { difTomoFrameNxs :: Nxs XrdOneD-- ^ nexus of the current frame
               , difTomoFrameIdx :: Int -- ^ index of the current frame
               , difTomoFrameEOF :: Bool -- ^ is it the eof of the stream
               , difTomoFrameGeometry :: Geometry -- ^ diffractometer geometry
               , difTomoFramePoniExt :: PoniExt -- ^ the ref poniext
               } deriving (Show)

class Frame t where
  len :: t -> IO (Maybe Int)
  row :: t -> Int -> MaybeT IO (DifTomoFrame DIM1)

instance Frame (DataFrameH5 XrdOneD) where
  len (DataFrameH5 _ _ _ (DataSourceH5 _ d) _ _) = lenH5Dataspace d

  row d@(DataFrameH5 nxs' _ g d' w ponigen) idx = do
    n <- lift $ len d
    let eof = fromJust n - 1 == idx
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- g `atIndex'` (ix1 0)
    delta <- d' `atIndex'` (ix1 idx)
    wavelength <- w `atIndex'` (ix1 0)
    let source = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    -- print positions
    let geometry =  Geometry K6c source positions Nothing
    let detector = ZeroD
    m <- lift $ geometryDetectorRotationGet geometry detector
    poniext <- lift $ ponigen (MyMatrix HklB m) idx
    return $ DifTomoFrame { difTomoFrameNxs = nxs'
                          , difTomoFrameIdx = idx
                          , difTomoFrameEOF = eof
                          , difTomoFrameGeometry = geometry
                          , difTomoFramePoniExt = poniext
                          }

-- type PipeE e a b m r = EitherT e (Pipe a b m) r

frames :: (Frame a) => Pipe a (DifTomoFrame DIM1) IO ()
frames = do
  d <- await
  (Just n) <- lift $ len d
  forM_ [0..n-1] (\i' -> do
                     f <- lift $ runMaybeT $ row d i'
                     when (isJust f) (yield (fromJust f)))

frames' :: (Frame a) => [Int] -> Pipe a (DifTomoFrame DIM1) IO ()
frames' is = do
  d <- await
  forM_ is (\i' -> do
              f <- lift $ runMaybeT $ row d i'
              when (isJust f) (yield (fromJust f)))

skip :: [Int] -> DifTomoFrame sh -> Bool
skip is' (DifTomoFrame _ i _ _ _) = notElem i is'

-- {-# ANN module "HLint: ignore Use camelCase" #-}


-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Diagrams

-- plotPonies :: FilePath -> [PoniEntry] -> IO ()
-- plotPonies f entries = toFile def f $ do
--     layout_title .= "Ponies"
--     setColors [opaque blue]
--     let values = map extract entries
--     plot (line "am" [values [0,(0.5)..400]])
--     -- plot (points "am points" (signal [0,7..400]))
--     where
--       extract (PoniEntry _ _ (Length poni1) _ _ _ _ _ _) = poni1

-- | Usual methods

getScanDir ∷ OutputBaseDir → FilePath → FilePath
getScanDir o f = o </> (dropExtension . takeFileName) f

pgen :: OutputBaseDir -> FilePath -> Int -> FilePath
pgen o f i = o </> scandir </>  scandir ++ printf "_%02d.poni" i
  where
    scandir = (dropExtension . takeFileName) f

getMEdf :: FilePath -> IO (MyMatrix Double)
getMEdf f = do
  edf <- edfFromFile f
  let mnes = map Text.pack ["_mu", "_keta", "_kap", "_kphi", "nu", "del"]
  let source = Source (edf'Lambda edf)
  let positions = fromList $ map (extract edf) mnes
  let geometry =  Geometry K6c source positions Nothing
  let detector = ZeroD
  m <- geometryDetectorRotationGet geometry detector
  return $ MyMatrix HklB m
    where
      extract :: Edf -> Text -> Double
      extract (Edf _ ms) key = fromMaybe 0.0 (List.lookup key ms)

poniFromFile :: FilePath -> IO Poni
poniFromFile filename = do
  content <- readFile filename
  return $ case parseOnly poniP content of
    Left _     -> error $ "Can not parse the " ++ filename ++ " poni file"
    Right poni -> poni

getPoniExtRef :: XRDRef -> IO PoniExt
getPoniExtRef (XRDRef _ output (XrdRefNxs nxs'@(Nxs f _) idx)) = do
  poniExtRefs <- runSafeT $
                 toListM ( withDataFrameH5 nxs' (gen output f) yield
                           >-> hoist lift ( frames' [idx]))
  return $ difTomoFramePoniExt (Prelude.last poniExtRefs)
  where
    gen :: FilePath -> FilePath -> MyMatrix Double -> Int -> IO PoniExt
    gen root nxs'' m idx' = do
      poni <- poniFromFile $ root </> scandir ++ printf "_%02d.poni" idx'
      return $ PoniExt poni m
      where
        scandir = takeFileName nxs''
getPoniExtRef (XRDRef _ _ (XrdRefEdf e p)) = do
  poni <- poniFromFile p
  m <- getMEdf e
  return $ PoniExt poni m

integrate ∷ XrdOneDParams a → XRDSample → IO ()
integrate p (XRDSample _ output nxss) = do
  _ <- mapConcurrently (integrate' p output) nxss
  return ()

integrate' ∷ XrdOneDParams a → OutputBaseDir → XrdNxs → IO ()
integrate' p output (XrdNxs b _ t is (XrdSourceNxs nxs'@(Nxs f _))) = do
  print f
  runSafeT $ runEffect $
    withDataFrameH5 nxs' (gen p) yield
    >-> hoist lift (frames
                    >-> filter (skip is)
                    >-> savePonies (pgen output f)
                    >-> savePy p b t
                    >-> saveGnuplot
                    >-> drain)
  where
    gen :: XrdOneDParams a -> Pose -> Int -> IO PoniExt
    gen (XrdOneDParams ref' _ _) m _idx = return $ setPose ref' m

createPy ∷ XrdOneDParams a → DIM1 → Threshold → FilePath → DifTomoFrame' sh → (Script Py2, FilePath)
createPy (XrdOneDParams _ mflat m) b (Threshold t) scriptPath (DifTomoFrame' f poniPath) = (Py2Script (script, scriptPath), output)
    where
      script = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from h5py import File"
                             , "from pyFAI import load"
                             , ""
                             , "PONIFILE = " ++ show poniPath
                             , "NEXUSFILE = " ++ show nxs'
                             , "IMAGEPATH = " ++ show i'
                             , "IDX = " ++ show idx
                             , "N = " ++ show (size b)
                             , "OUTPUT = " ++ show output
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "# load the flat"
                             , "flat = " ++ flatValueForPy mflat
                             , ""
                             , "ai = load(PONIFILE)"
                             , "ai.wavelength = WAVELENGTH"
                             , "ai._empty = numpy.nan"
                             , "mask_det = ai.detector.mask"
                             , "mask_module = numpy.zeros_like(mask_det, dtype=bool)"
                             , "#mask_module[0:50, :] = True"
                             , "#mask_module[910:960, :] = True"
                             , "#mask_module[:,0:10] = True"
                             , "mask_module[:,550:] = True"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    img = f[IMAGEPATH][IDX]"
                             , "    mask = numpy.where(img > THRESHOLD, True, False)"
                             , "    #mask = numpy.logical_or(mask, mask_det)"
                             , "    mask = numpy.logical_or(mask, mask_module)"
                             , "    ai.integrate1d(img, N, filename=OUTPUT, unit=\"2th_deg\", error_model=\"poisson\", correctSolidAngle=False, method=\"" ++ show m ++ "\", mask=mask, flat=flat)"
                             ]
      (Nxs nxs' (XrdOneDH5Path (DataItemH5 i' _) _ _ _)) = difTomoFrameNxs f
      idx = difTomoFrameIdx f
      output = poniPath `replaceExtension` "dat"
      (Geometry _ (Source w) _ _) = difTomoFrameGeometry f

-- | Pipes

data DifTomoFrame' sh = DifTomoFrame' { difTomoFrame'DifTomoFrame :: DifTomoFrame sh
                                      , difTomoFrame'PoniPath :: FilePath
                                      }

savePonies :: (Int -> FilePath) -> Pipe (DifTomoFrame sh) (DifTomoFrame' sh) IO ()
savePonies g = forever $ do
  f <- await
  let filename = g (difTomoFrameIdx f)
  let (PoniExt p _) = difTomoFramePoniExt f
  lift $ filename `hasContent` (poniToText p)
  yield $ DifTomoFrame' { difTomoFrame'DifTomoFrame = f
                        , difTomoFrame'PoniPath = filename
                        }

data DifTomoFrame'' sh = DifTomoFrame'' { difTomoFrame''DifTomoFrame' :: DifTomoFrame' sh
                                        , difTomoFrame''PySCript :: Script Py2
                                        , difTomoFrame''DataPath :: FilePath
                                        }

savePy ∷ XrdOneDParams a → DIM1 → Threshold → Pipe (DifTomoFrame' sh) (DifTomoFrame'' sh) IO ()
savePy p b t = forever $ do
  f@(DifTomoFrame' _difTomoFrame poniPath) <- await
  let scriptPath = poniPath `replaceExtension`"py"
  let (script, dataPath) = createPy p b t scriptPath f
  ExitSuccess <- lift $ run script True
  yield $ DifTomoFrame'' { difTomoFrame''DifTomoFrame' = f
                         , difTomoFrame''PySCript = script
                         , difTomoFrame''DataPath = dataPath
                         }

data DifTomoFrame''' sh = DifTomoFrame''' { difTomoFrame'''DifTomoFrame'' ∷ DifTomoFrame'' sh
                                          , difTomoFrame'''GnuplotScript ∷ Script Gnuplot
                                          , difTomoFrame'''Curves ∷ [FilePath]
                                          }

saveGnuplot' :: Pipe (DifTomoFrame'' sh) (DifTomoFrame''' sh) (StateT [FilePath] IO) r
saveGnuplot' = forever $ do
  curves <- lift get
  f@(DifTomoFrame'' (DifTomoFrame' _ poniPath) _ dataPath) <- await
  let script = ScriptGnuplot (new_content curves, takeDirectory poniPath </> "plot.gnuplot")
  let curves' = curves ++ [dataPath]
  lift . lift $ scriptSave script
  lift $ put $! curves'
  yield $ DifTomoFrame''' { difTomoFrame'''DifTomoFrame'' = f
                          , difTomoFrame'''GnuplotScript = script
                          , difTomoFrame'''Curves = curves'
                          }
    where
      new_content :: [FilePath] -> Text
      new_content cs = Text.unlines (lines' cs)

      lines' :: [FilePath] -> [Text]
      lines' cs = ["plot \\"]
                 ++ [Text.intercalate ",\\\n" [ Text.pack (show (takeFileName c) ++ " u 1:2 w l") | c <- cs ]]
                 ++ ["pause -1"]

saveGnuplot :: Pipe (DifTomoFrame'' sh) (DifTomoFrame''' sh) IO r
saveGnuplot = evalStateP [] saveGnuplot'

-- substract a sample from another one

targetP ∷ (Int → FilePath) → Pipe (DifTomoFrame sh) FilePath IO ()
targetP g = forever $ do
  f ← await
  let poniPath = g (difTomoFrameIdx f)
  let dataPath = poniPath `replaceExtension` "dat"
  yield dataPath

target' ∷  XrdOneDParams a → OutputBaseDir → XrdNxs → IO (FilePath, [FilePath])
target' p output (XrdNxs _ _ _ is (XrdSourceNxs nxs'@(Nxs f _))) = do
  fs ← runSafeT $ toListM $
       withDataFrameH5 nxs' (gen p) yield
       >-> hoist lift (frames
                       >-> filter (skip is)
                       >-> targetP (pgen output f)
                      )
  return (getScanDir output f, fs)
  where
    gen :: XrdOneDParams a -> Pose -> Int -> IO PoniExt
    gen (XrdOneDParams ref' _ _) m _idx = return $ setPose ref' m

targets ∷ XrdOneDParams a → XRDSample → IO [(FilePath, [FilePath])]
targets p (XRDSample _ output nxss) = mapConcurrently (target' p output) nxss

substract' ∷ XrdOneDParams a → XRDSample → XRDSample → IO ()
substract' p s1@(XRDSample name _ _) s2 = do
  -- compute the output of the s1 sample
  -- we take only the first list of the sample
  f1s:_ ← targets p s1
  -- compute the output of the s2 sample
  f2s ← targets p s2
  -- do the substraction via a python script.
  _ ← mapConcurrently (go f1s) f2s

  return ()
  where
    go ∷ (FilePath, [FilePath]) → (FilePath, [FilePath]) → IO ()
    go (_, f1) (d, f2) = do
      -- compute the substracted output file names
      let outputs = [dropExtension f ++ "-" ++ name <.> "dat" | f ← f2]
      -- compute the script name
      let scriptPath = d </> "substract.py"
      let script = script' f1 f2 outputs scriptPath
      print scriptPath
      ExitSuccess ← run script False
      return ()


    script' ∷ [FilePath] → [FilePath] → [FilePath] → FilePath → Script Py2
    script' fs1 fs2 os scriptPath = Py2Script (content, scriptPath)
      where
        content ∷ Text
        content = Text.unlines $
              map Text.pack ["#!/bin/env python"
                            , ""
                            , "import numpy"
                            , ""
                            , "S1 = [" ++ List.intercalate ",\n" (map show fs1) ++ "]"
                            , "S2 = [" ++ List.intercalate ",\n" (map show fs2) ++ "]"
                            , "OUTPUTS = [" ++ List.intercalate ",\n" (map show os) ++ "]"
                            , ""
                            , "def substract(f1, f2, o):"
                            , "    a1 = numpy.genfromtxt(f1)"
                            , "    a2 = numpy.genfromtxt(f2)"
                            , "    res = numpy.copy(a2)"
                            , "    res[:,1] -= a1[:,1]"
                            , "    # TODO deal with the error propagation"
                            , "    numpy.savetxt(output, res)"
                            , ""
                            , "for (s1, s2, output) in zip(S1, S2, OUTPUTS):"
                            , "    substract(s1, s2, output)"
                            ]

substract ∷  XrdOneDParams a → XRDSample → [XRDSample] → IO ()
substract p s ss = mapM_ (substract' p s) ss

-- | PyFAI MultiGeometry

integrateMulti ∷ XrdOneDParams a → XRDSample → IO ()
integrateMulti p (XRDSample _ output nxss) =
  mapM_ (integrateMulti' p output) nxss

integrateMulti' ∷ XrdOneDParams a → OutputBaseDir → XrdNxs → IO ()
integrateMulti' p output (XrdNxs _ mb t is (XrdSourceNxs nxs'@(Nxs f _))) = do
  print f
  runSafeT $ runEffect $
    withDataFrameH5 nxs' (gen p) yield
    >-> hoist lift (frames
                    >-> filter (skip is)
                    >-> savePonies (pgen output f)
                    >-> saveMultiGeometry p mb t)
  where
    gen :: XrdOneDParams a -> Pose -> Int -> IO PoniExt
    gen (XrdOneDParams ref' _ _)  m _idx = return $ setPose ref' m

integrateMulti' p output (XrdNxs b _ t _ (XrdSourceEdf fs)) = do
  -- generate all the ponies
  zipWithM_ (go p) fs ponies

  -- generate the multi.py python script
  let scriptPath = output </> "multi.py"
  let (script, _) = createMultiPyEdf p b t fs ponies scriptPath (output </> "multi.dat")
  scriptSave script
    where
      ponies = [output </> (dropExtension . takeFileName) f ++ ".poni" | f <- fs]

      go ∷ XrdOneDParams a → FilePath → FilePath → IO ()
      go (XrdOneDParams ref _ _) f o = do
        m <- getMEdf f
        let (PoniExt p' _) = setPose ref m
        o `hasContent` (poniToText p')

createMultiPy ∷ XrdOneDParams a → DIM1 → Threshold → FilePath → DifTomoFrame' sh → [(Int, FilePath)] → (Script Py2, FilePath)
createMultiPy (XrdOneDParams _ mflat _) b (Threshold t) scriptPath (DifTomoFrame' f _) idxPonies = (Py2Script (content, scriptPath), output)
    where
      content = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from h5py import File"
                             , "from pyFAI.multi_geometry import MultiGeometry"
                             , ""
                             , "NEXUSFILE = " ++ show nxs'
                             , "IMAGEPATH = " ++ show i'
                             , "BINS = " ++ show (size b)
                             , "OUTPUT = " ++ show output
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "# load the flat"
                             , "flat = " ++ flatValueForPy mflat
                             , ""
                             , "# Load all images"
                             , "PONIES = [" ++ List.intercalate ",\n" (map show ponies) ++ "]"
                             , "IDXS = [" ++ List.intercalate ", " (map show idxs) ++ "]"
                             , ""
                             , "# Read all the images"
                             , "imgs = []"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    for idx in IDXS:"
                             , "        imgs.append(f[IMAGEPATH][idx])"
                             , ""
                             , "# Compute the mask"
                             , "mask = numpy.zeros_like(imgs[0], dtype=bool)"
                             , "mask[:,550:] = True"
                             , "if flat is not None:  # this should be removed for pyFAI >= 0.13.1 it is now done by PyFAI"
                             , "    mask = numpy.logical_or(mask, flat == 0.0)"
                             , "lst_mask = []"
                             , "for img in imgs:  # remove all pixels above the threshold"
                             , "    mask_t = numpy.where(img > THRESHOLD, True, False)"
                             , "    lst_mask.append(numpy.logical_or(mask, mask_t))"
                             , ""
                             , "# Integration multi-geometry 1D"
                             , "mg = MultiGeometry(PONIES, unit=\"2th_deg\", radial_range=(0,80))"
                             , "p = mg.integrate1d(imgs, BINS, lst_mask=lst_mask, lst_flat=flat)"
                             , ""
                             , "# Save the datas"
                             , "numpy.savetxt(OUTPUT, numpy.array(p).T)"
                             ]
      (Nxs nxs' (XrdOneDH5Path (DataItemH5 i' _) _ _ _)) = difTomoFrameNxs f
      output = "multi.dat"
      (Geometry _ (Source w) _ _) = difTomoFrameGeometry f
      (idxs, ponies) = unzip idxPonies

createMultiPyEdf ∷ XrdOneDParams a → DIM1 → Threshold → [FilePath] → [FilePath] → FilePath → FilePath → (Script Py2, FilePath)
createMultiPyEdf (XrdOneDParams _ mflat _) b (Threshold t) edfs ponies scriptPath output = (Py2Script (content, scriptPath), output)
    where
      content = Text.unlines $
                map Text.pack ["#!/bin/env python"
                              , ""
                              , "import numpy"
                              , "from fabio import open"
                              , "from pyFAI.multi_geometry import MultiGeometry"
                              , ""
                              , "EDFS = [" ++ List.intercalate ",\n" (map show edfs) ++ "]"
                              , "PONIES = [" ++ List.intercalate ",\n" (map show ponies) ++ "]"
                              , "BINS = " ++ show (size b)
                              , "OUTPUT = " ++ show output
                              , "THRESHOLD = " ++ show t
                              , ""
                              , "# load the flat"
                              , "flat = " ++ flatValueForPy mflat
                              , ""
                              , "# Read all the images"
                              , "imgs = [open(edf).data for edf in EDFS]"
                              , ""
                              , "# Compute the mask"
                              , "mask = numpy.zeros_like(imgs[0], dtype=bool)"
                              , "for img in imgs:"
                              , "    mask_t = numpy.where(img > THRESHOLD, True, False)"
                              , "    mask = numpy.logical_or(mask, mask_t)"
                              , ""
                              , "# Integration multi-geometry 1D"
                              , "mg = MultiGeometry(PONIES, unit=\"2th_deg\", radial_range=(0,80))"
                              , "p = mg.integrate1d(imgs, BINS, lst_mask=mask)"
                              , ""
                              , "# Save the datas"
                              , "numpy.savetxt(OUTPUT, numpy.array(p).T)"
                              ]

saveMulti' ∷ XrdOneDParams a → DIM1 → Threshold → Consumer (DifTomoFrame' sh) (StateT [(Int, FilePath)] IO) r
saveMulti' p b t = forever $ do
  idxPonies <- lift get
  f'@(DifTomoFrame' f@(DifTomoFrame _ idx _ _ _) poniPath) <- await
  let directory = takeDirectory poniPath
  let filename = directory </> "multi.py"
  let (script, _) = createMultiPy p b t filename f' idxPonies
  ExitSuccess ← lift . lift $ if (difTomoFrameEOF f) then (run script True) else return ExitSuccess
  lift $ put $! (idxPonies ++ [(idx, poniPath)])

saveMultiGeometry ∷ XrdOneDParams a → DIM1 → Threshold → Consumer (DifTomoFrame' sh) IO r
saveMultiGeometry p b t = evalStateP [] (saveMulti' p b t)
