{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Hkl.Xrd.Mesh
       ( XrdMeshSample(..)
       , XrdMesh(..)
       , XrdMeshSource(..)
       , XrdMeshH5Path(..)
       , integrateMesh
       , mkXrdMeshNxs
       , mkXrdMeshSourceNxs
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception.Base (bracket)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array.Repa (Shape, DIM1, ix1, size)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as Text (unlines, pack)
import Data.Vector.Storable (Vector, any, concat, head, singleton)
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ((</>), dropExtension, takeFileName)

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
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.Types
import Hkl.Utils
import Hkl.Xrd.OneD

-- | Types

data XrdMeshH5Path a b c d e f = XrdMeshH5Path
                                 (DataItem a) -- ^ Image
                                 (DataItem b) -- ^ meshx
                                 (DataItem c) -- ^ meshy
                                 (DataItem d) -- ^ gamma
                                 (DataItem e) -- ^ delta
                                 (DataItem f) -- ^ wavelength
                               deriving (Show)

data XrdMeshH5 b c d e f = XrdMeshH5
                           (DataSource b) -- ^ MeshX
                           (DataSource c) -- ^ MeshY
                           (DataSource d) -- ^ Gamma
                           (DataSource e) -- ^ Delta
                           (DataSource f) -- ^ Wavelength

data XrdMeshSample a b c d e f = XrdMeshSample SampleName OutputBaseDir [XrdMesh a b c d e f] -- ^ nxss

data XrdMesh a b c d e f = XrdMesh DIM1 DIM1 Threshold (XrdMeshSource a b c d e f) deriving (Show)

data XrdMeshSource a b c d e f = XrdMeshSourceNxs (XrdMeshNxs a b c d e f)
                               | XrdMeshSourceNxsFly [XrdMeshNxs a b c d e f]
                               deriving (Show)

data XrdMeshNxs a b c d e f = XrdMeshNxs FilePath NxEntry (XrdMeshH5Path a b c d e f)
                  deriving (Show)

mkXrdMeshNxs :: FilePath -> NxEntry -> (NxEntry -> XrdMeshH5Path a b c d e f) -> (XrdMeshNxs a b c d e f)
mkXrdMeshNxs f e h = XrdMeshNxs f e (h e)

mkXrdMeshSourceNxs :: FilePath -> NxEntry -> (NxEntry -> XrdMeshH5Path a b c d e f) -> (XrdMeshSource a b c d e f)
mkXrdMeshSourceNxs f e h = XrdMeshSourceNxs $ mkXrdMeshNxs f e h

data XrdMeshFrame = XrdMeshFrame
                    WaveLength
                    (MyMatrix Double)
                  deriving (Show)

class FrameND t where
  rowND :: t -> MaybeT IO XrdMeshFrame

instance FrameND (XrdMeshH5 a b c d e) where

  rowND (XrdMeshH5 _ _ g d w) = do
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
    return $ XrdMeshFrame w' (MyMatrix HklB m)
    where
      get_position' :: Shape sh => DataSource a -> sh -> MaybeT IO (Vector Double)
      get_position' (DataSourceH5 _ a ) b = lift $ do
        v <- get_position_new a b
        if any isNaN v then fail "File contains Nan" else return v
      get_position' (DataSourceConst v) _ = lift $ return $ singleton v

withDataSource :: File -> (XrdMeshH5Path a b c d e f) -> (XrdMeshH5 b c d e f -> IO r) -> IO r
withDataSource h s = bracket (before s) after
    where
      before :: (XrdMeshH5Path a b c d e f) -> IO (XrdMeshH5 b c d e f)
      before (XrdMeshH5Path _i x y g d w) =
          XrdMeshH5 <$> openDataset' h x
                    <*> openDataset' h y
                    <*> openDataset' h g
                    <*> openDataset' h d
                    <*> openDataset' h w


      after :: XrdMeshH5 b c d e f -> IO ()
      after (XrdMeshH5 x' y' g' d' w') = do
        releaseDataSource x'
        releaseDataSource y'
        releaseDataSource g'
        releaseDataSource d'
        releaseDataSource w'

      openDataset' :: File -> DataItem a -> IO (DataSource a)
      openDataset' hid di@(DataItemH5 name _) = DataSourceH5
                                                <$> return di
                                                <*> openDataset hid (Char8.pack name) Nothing
      openDataset' _ (DataItemConst v) = return $ DataSourceConst v

      releaseDataSource :: DataSource a -> IO ()
      releaseDataSource (DataSourceH5 _ d) = closeDataset d
      releaseDataSource (DataSourceConst _) = return ()

xrdMeshPy :: FilePath -> FilePath -> String -> String -> String -> DIM1 -> Threshold -> WaveLength -> FilePath -> FilePath -> (Text, FilePath)
xrdMeshPy p f x y i b (Threshold t) w o os = (script, os)
    where
      script = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from h5py import File"
                             , "from pyFAI import load"
                             , ""
                             , "PONIFILE = " ++ show p
                             , "NEXUSFILE = " ++ show f
                             , "MESHX = " ++ show x
                             , "MESHY = " ++ show y
                             , "IMAGEPATH = " ++ show i
                             , "N = " ++ show (size b)
                             , "OUTPUT = " ++ show o
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "ai = load(PONIFILE)"
                             , "ai.wavelength = WAVELENGTH"
                             , "ai._empty = numpy.nan"
                             , "mask_det = ai.detector.mask"
                             , "mask_module = numpy.zeros_like(mask_det, dtype=bool)"
                             , "mask_module[0:50, :] = True"
                             , "mask_module[910:960, :] = True"
                             , "mask_module[:,0:50] = True"
                             , "mask_module[:,510:560] = True"
                             , "mask_det = numpy.logical_or(mask_det, mask_module)"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    nx = f[MESHX].shape[0]"
                             , "    ny = f[MESHY].shape[0]"
                             , "    imgs = f[IMAGEPATH]"
                             , "    with File(OUTPUT, mode='w') as o:"
                             , "        o.create_dataset('map', shape=(ny, nx, N), dtype='float')"
                             , "        for y in range(ny):"
                             , "            for x in range(nx):"
                             , "                img = imgs[y, x]"
                             , "                mask = numpy.where(img > THRESHOLD, True, False)"
                             , "                mask = numpy.logical_or(mask, mask_det)"
                             , "                tth, I, sigma = ai.integrate1d(img, N, unit=\"2th_deg\", error_model=\"poisson\", correctSolidAngle=False, method=\"csr_ocl\", mask=mask, safe=False)"
                             , "                o['map'][y, x] = I"
                             ]

xrdMeshFlyPy :: FilePath -> [FilePath] -> String -> String -> String -> DIM1 -> Threshold -> WaveLength -> FilePath -> FilePath -> (Text, FilePath)
xrdMeshFlyPy p fs x y i b (Threshold t) w o os = (script, os)
    where
      script = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from h5py import File"
                             , "from pyFAI import load"
                             , ""
                             , "PONIFILE = " ++ show p
                             , "NEXUSFILE = [" ++ intercalate ",\n" (map show fs) ++ "]"
                             , "MESHX = " ++ show x
                             , "MESHY = " ++ show y
                             , "IMAGEPATH = " ++ show i
                             , "N = " ++ show (size b)
                             , "OUTPUT = " ++ show o
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "ai = load(PONIFILE)"
                             , "ai.wavelength = WAVELENGTH"
                             , "ai._empty = numpy.nan"
                             , "mask_det = ai.detector.mask"
                             , "mask_module = numpy.zeros_like(mask_det, dtype=bool)"
                             , "mask_module[0:50, :] = True"
                             , "mask_module[910:960, :] = True"
                             , "mask_module[:,0:50] = True"
                             , "mask_module[:,510:560] = True"
                             , "mask_det = numpy.logical_or(mask_det, mask_module)"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    nx = f[MESHX].shape[0]"
                             , "    ny = f[MESHY].shape[0]"
                             , "    imgs = f[IMAGEPATH]"
                             , "    with File(OUTPUT, mode='w') as o:"
                             , "        o.create_dataset('map', shape=(ny, nx, N), dtype='float')"
                             , "        for y in range(ny):"
                             , "            for x in range(nx):"
                             , "                img = imgs[y, x]"
                             , "                mask = numpy.where(img > THRESHOLD, True, False)"
                             , "                mask = numpy.logical_or(mask, mask_det)"
                             , "                tth, I, sigma = ai.integrate1d(img, N, unit=\"2th_deg\", error_model=\"poisson\", correctSolidAngle=False, method=\"csr_ocl\", mask=mask, safe=False)"
                             , "                o['map'][y, x] = I"
                             ]

getWaveLengthAndPoniExt :: PoniExt -> XrdMeshSource a b c d e f -> IO (WaveLength, PoniExt)
getWaveLengthAndPoniExt ref (XrdMeshSourceNxs (XrdMeshNxs f _ h5path)) =
    withH5File f $ \h5file ->
        withDataSource h5file h5path $ \h -> do
          -- read the first frame and get the poni used for all the integration.
          d <- runMaybeT $ rowND h
          let (XrdMeshFrame w m) = fromJust d
          let poniext = setPose ref m
          return (w, poniext)
getWaveLengthAndPoniExt ref (XrdMeshSourceNxsFly (XrdMeshNxs f _ h5path:_)) =
    withH5File f $ \h5file ->
      withDataSource h5file h5path $ \h -> do
        -- read the first frame and get the poni used for all the integration.
        d <- runMaybeT $ rowND h
        let (XrdMeshFrame w m) = fromJust d
        let poniext = setPose ref m
        return (w, poniext)

integrateMesh :: PoniExt -> (XrdMeshSample a b c d e f) -> IO ()
integrateMesh ref (XrdMeshSample _ output nxss) =
  mapM_ (integrateMesh' ref output) nxss

integrateMesh' :: PoniExt -> OutputBaseDir -> (XrdMesh a b c d e f) -> IO ()
integrateMesh' ref output (XrdMesh b _ t nxs'@(XrdMeshSourceNxs (XrdMeshNxs f _ h5path))) = do
    -- get the poniext for all the scan
    (w, (PoniExt p _)) <- getWaveLengthAndPoniExt ref nxs'

    -- save the poni at the right place.
    let sdir = (dropExtension . takeFileName) f
    let pfilename = output </> sdir </> sdir ++ ".poni"
    saveScript (poniToText p) pfilename

    -- create the python script to do the integration.
    let (XrdMeshH5Path (DataItemH5 i _) (DataItemH5 x _) (DataItemH5 y _) _ _ _) = h5path
    let o = output </> sdir </> sdir ++ ".h5"
    let os = output </> sdir </> sdir ++ ".py"
    let (script, scriptPath) = xrdMeshPy pfilename f x y i b t w o os

    -- save the script
    saveScript script scriptPath

    -- run the script
    ExitSuccess <- runPythonScript scriptPath False

    return ()
integrateMesh' ref output (XrdMesh b _ t ss) = do
    -- get the poniext for all the scan
    (w, PoniExt p _) <- getWaveLengthAndPoniExt ref ss

    -- save this poni at the right place
    let (XrdMeshSourceNxsFly (XrdMeshNxs _ nxentry h5path:_)) = ss
    let pfilename = output </> nxentry </> nxentry ++ ".poni"
    saveScript (poniToText p) pfilename

    -- create the python script to do the integration
    let (XrdMeshH5Path (DataItemH5 i _) (DataItemH5 x _) (DataItemH5 y _) _ _ _) = h5path
    let o = output </> nxentry </> nxentry ++ ".h5"
    let os = output </> nxentry </> nxentry ++ ".py"

    -- let (script, scriptPath) = xrdMeshFlyPy
    -- save it
    -- run it
    return ()
