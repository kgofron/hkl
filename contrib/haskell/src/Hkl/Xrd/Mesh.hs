{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Xrd.Mesh
       ( XrdMeshSample(..)
       , XrdMesh'(..)
       , XrdMeshParams(..)
       , XrdMeshSource(..)
       , integrateMesh
       ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array.Repa (Shape, DIM1, ix1, size)
import Data.Maybe (fromJust)
import Data.Vector.Storable (Vector, any, concat, head, singleton)
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ((</>), (<.>), dropExtension, splitDirectories, takeFileName)

import qualified Data.Text as Text (unlines, pack)

import Prelude hiding
    ( any
    , concat
    , head
    , lookup
    , readFile
    , unlines
    )
import Pipes ( lift )

import Hkl.C
import Hkl.DataSource
import Hkl.Detector
import Hkl.Flat
import Hkl.H5
import Hkl.PyFAI
import Hkl.Python
import Hkl.MyMatrix
import Hkl.Nxs
import Hkl.Script
import Hkl.Types
import Hkl.Utils
import Hkl.Xrd.OneD

--  Types

data XrdMeshSource  = XrdMeshSourceNxs (Nxs XrdMesh)
                    | XrdMeshSourceNxsFly [Nxs XrdMesh]
                    deriving (Show)

data XrdMesh' = XrdMesh DIM1 DIM1 (Maybe Threshold) XrdMeshSource deriving (Show)

data XrdMeshSample = XrdMeshSample SampleName AbsDirPath [XrdMesh'] deriving (Show) --  nxss

data XrdMeshParams a = XrdMeshParams PoniExt (Maybe (Flat a)) AIMethod

data XrdMeshFrame = XrdMeshFrame
                    WaveLength
                    Pose
                  deriving (Show)

class FrameND t where
  rowND :: t -> MaybeT IO XrdMeshFrame

instance FrameND (DataFrameH5 XrdMesh) where

  rowND (XrdMeshH5 _ _ _ _ _ g d w) = do
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- get_position' g (ix1 0)
    delta <- get_position' d (ix1 0)
    wavelength <- get_position' w (ix1 0)
    let source@(Source w') = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    let geometry =  Geometry K6c source positions Nothing
    let detector = ZeroD
    m <- lift $ geometryDetectorRotationGet geometry detector
    let pose = Pose (MyMatrix HklB m)
    return $ XrdMeshFrame w' pose
    where
      get_position' :: Shape sh => DataSource a -> sh -> MaybeT IO (Vector Double)
      get_position' (DataSourceH5 _ a ) b = lift $ do
        v <- get_position_new a b
        if any isNaN v then fail "File contains Nan" else return v
      get_position' (DataSourceConst v) _ = lift $ return $ singleton v

  rowND (XrdMeshFlyH5 _ _ _ _ _ g d w) = do
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- get_position' g (ix1 0)
    delta <- get_position' d (ix1 0)
    wavelength <- get_position' w (ix1 0)
    let source@(Source w') = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    let geometry =  Geometry K6c source positions Nothing
    let detector = ZeroD
    m <- lift $ geometryDetectorRotationGet geometry detector
    let pose = Pose (MyMatrix HklB m)
    return $ XrdMeshFrame w' pose
    where
      get_position' :: Shape sh => DataSource a -> sh -> MaybeT IO (Vector Double)
      get_position' (DataSourceH5 _ a ) b = lift $ do
        v <- get_position_new a b
        if any isNaN v then fail "File contains Nan" else return v
      get_position' (DataSourceConst v) _ = lift $ return $ singleton v

integrateMesh ∷  XrdMeshParams a → [XrdMeshSample] → IO ()
integrateMesh p ss = void $ mapConcurrently (integrateMesh' p) ss

integrateMesh' ∷ XrdMeshParams a → XrdMeshSample → IO ()
integrateMesh' p (XrdMeshSample _ output nxss) = mapM_ (integrateMesh'' p output) nxss

getWaveLengthAndPoniExt' ∷ XrdMeshParams a → Nxs XrdMesh → IO (WaveLength, PoniExt)
getWaveLengthAndPoniExt' (XrdMeshParams ref _ _) nxs =
   withDataSource nxs $ \h -> do
    -- read the first frame and get the poni used for all the integration.
    d <- runMaybeT $ rowND h
    let (XrdMeshFrame w p) = fromJust d
    let poniext = move ref p
    return (w, poniext)

getWaveLengthAndPoniExt ∷ XrdMeshParams a → XrdMeshSource → IO (WaveLength, PoniExt)
getWaveLengthAndPoniExt p (XrdMeshSourceNxs nxs) = getWaveLengthAndPoniExt' p nxs
getWaveLengthAndPoniExt p (XrdMeshSourceNxsFly (nxs:_)) = getWaveLengthAndPoniExt' p nxs
getWaveLengthAndPoniExt _ (XrdMeshSourceNxsFly []) = error "getWaveLengthAndPoniExt"

getOutputPath' ∷ AbsDirPath → FilePath → (FilePath, FilePath, FilePath)
getOutputPath' o d = (poni, h5, py)
  where
    poni = o </> d </> d <.> "poni"
    h5 = o </> d </> d <.> "h5"
    py =  o </> d </> d <.> "py"

getOutputPath ∷ AbsDirPath → XrdMeshSource → (FilePath, FilePath, FilePath)
getOutputPath o (XrdMeshSourceNxs (Nxs f _)) = getOutputPath' o dir
  where
    dir ∷ FilePath
    dir =  (dropExtension . takeFileName) f
getOutputPath o (XrdMeshSourceNxsFly (Nxs _ h:_)) = getOutputPath' o dir
  where
    (XrdMeshFlyH5Path (DataItemH5 i _) _ _ _ _ _) = h
    dir:_ = splitDirectories i
getOutputPath _ (XrdMeshSourceNxsFly []) = error "getOutputPath"


xrdMeshPy'' ∷ Maybe (Flat a)
            → AIMethod -- pyFAI azimuthal integration method
            → [FilePath] -- nexus files
            → H5Path -- image path
            → H5Path -- meshx path
            → H5Path -- meshy path
            → FilePath -- ponipath
            → DIM1 -- bins
            → Maybe Threshold -- threshold
            → WaveLength -- wavelength
            → FilePath -- output h5
            → FilePath -- script name
            → Script Py2
xrdMeshPy'' mflat m fs i x y p b mt w o scriptPath = Py2Script (content, scriptPath)
    where
      content = Text.unlines $
                map Text.pack ["#!/bin/env python"
                              , ""
                              , "import itertools"
                              , "import numpy"
                              , "from h5py import File"
                              , "from pyFAI import load"
                              , ""
                              , "PONIFILE = " ++ toPyVal p
                              , "NEXUSFILES = " ++ toPyVal fs
                              , "MESHX = " ++ toPyVal x
                              , "MESHY = " ++ toPyVal y
                              , "IMAGEPATH = " ++ toPyVal i
                              , "N = " ++ toPyVal (size b)
                              , "OUTPUT = " ++ toPyVal o
                              , "WAVELENGTH = " ++ toPyVal (w /~ meter)
                              , ""
                              , "# Load the flat"
                              , "flat = " ++ toPyVal mflat
                              , ""
                              , "# Load and prepare the common Azimuthal Integrator"
                              , "ai = load(PONIFILE)"
                              , "ai.wavelength = WAVELENGTH"
                              , "ai._empty = numpy.nan"
                              , ""
                              , "# Compute the fix part of the mask"
                              , "mask = numpy.zeros_like(ai.detector.mask, dtype=bool)"
                              , "mask[0:50, :] = True"
                              , "mask[910:960, :] = True"
                              , "mask[:,0:50] = True"
                              , "mask[:,510:560] = True"
                              , "if flat is None:"
                              , "    mask = numpy.logical_or(mask, ai.detector.mask)"
                              , ""
                              , dummiesForPy mt
                              , ""
                              , "# Compute the size of the output"
                              , "FS = [File(n, mode='r') for n in NEXUSFILES]"
                              , "NX = 0"
                              , "NY = 0"
                              , "for f in FS:"
                              , "    NX  = f[MESHX].shape[1]"
                              , "    NY += f[MESHY].shape[0]"
                              , ""
                              , "def gen(fs):"
                              , "    for f in fs:"
                              , "        for i in f[IMAGEPATH]:"
                              , "            yield i"
                              , ""
                              , "# Create and fill the ouput file"
                              , "with File(OUTPUT, mode='w') as o:"
                              , "    dataset = o.create_dataset('map', shape=(NY, NX, N), dtype='float')"
                              , "    lines = gen(FS)"
                              , "    for j, line in enumerate(lines):"
                              , "       for i, img in enumerate(line):"
                              , "            tth, I, sigma = ai.integrate1d(img, N, unit=\"2th_deg\","
                              , "                                           error_model=\"poisson\", correctSolidAngle=False,"
                              , "                                           method=\"" ++ show m ++ "\","
                              , "                                           mask=mask,"
                              , "                                           dummy=DUMMY, delta_dummy=DELTA_DUMMY,"
                              , "                                           safe=False, flat=flat)"
                              , "            dataset[j, i] = I"
                              ]

xrdMeshPy' ∷ XrdMeshParams a
           → XrdMeshSource -- data source
           → FilePath -- ponipath
           → DIM1 -- bins
           → Maybe Threshold -- threshold
           → WaveLength -- wavelength
           → FilePath -- output h5
           → FilePath -- script name
           → Script Py2
xrdMeshPy' (XrdMeshParams _ mflat m) (XrdMeshSourceNxs (Nxs f h5path)) p b mt w o scriptPath =
  xrdMeshPy'' mflat m [f] i x y p b mt w o scriptPath
  where
      (XrdMeshH5Path (DataItemH5 i _) (DataItemH5 x _) (DataItemH5 y _) _ _ _) = h5path
xrdMeshPy' (XrdMeshParams _ mflat m) (XrdMeshSourceNxsFly nxss) p b mt w o scriptPath =
  xrdMeshPy'' mflat m fs i x y p b mt w o scriptPath
  where
    fs ∷ [FilePath]
    fs = [f | (Nxs f _) ← nxss]

    Nxs _ h5path:_ = nxss

    (XrdMeshFlyH5Path (DataItemH5 i _) (DataItemH5 x _) (DataItemH5 y _) _ _ _) = h5path

integrateMesh'' ∷ XrdMeshParams a → AbsDirPath → XrdMesh' → IO ()
integrateMesh'' p' output (XrdMesh b _ mt s) = do
    -- get the poniext for all the scan
    (w, PoniExt p _) <- getWaveLengthAndPoniExt p' s

    -- save this poni at the right place
    let (ponipath, h5, py) = getOutputPath output s
    ponipath `hasContent` poniToText p

    -- create the python script to do the integration
    let script = xrdMeshPy' p' s ponipath b mt w h5 py
    ExitSuccess ← run script False

    return ()
