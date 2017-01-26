{-# LANGUAGE CPP #-}

module Hkl.Xrd.Mesh
       ( XrdMeshSample(..)
       , XrdMesh(..)
       , XrdMeshSource(..)
       , XrdMeshH5Path(..)
       , Nxs'(..)
       , integrateMesh
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
import qualified Data.Text as Text (unlines, pack)
import Data.Vector.Storable (Vector, any, concat, head)
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
import Hkl.Detector
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
import Hkl.Types
import Hkl.Utils
import Hkl.Xrd.OneD

-- | Types

data XrdMeshH5Path = XrdMeshH5Path
                     DataItem -- ^ Image
                     DataItem -- ^ meshx
                     DataItem -- ^ meshy
                     DataItem -- ^ gamma
                     DataItem -- ^ delta
                     DataItem -- ^ wavelength
                   deriving (Show)

data XrdMeshH5 = XrdMeshH5
                 Dataset -- ^ MeshX
                 Dataset -- ^ MeshY
                 Dataset -- ^ Gamma
                 Dataset -- ^ Delta
                 Dataset -- ^ Wavelength

data XrdMeshSample = XrdMeshSample SampleName OutputBaseDir [XrdMesh] -- ^ nxss

data XrdMesh = XrdMesh DIM1 DIM1 Threshold XrdMeshSource deriving (Show)

data XrdMeshSource = XrdMeshSourceNxs Nxs'
                   deriving (Show)

data Nxs' = Nxs' FilePath NxEntry XrdMeshH5Path
             deriving (Show)

mkXrdMeshSourceNxs :: FilePath -> NxEntry -> (NxEntry -> XrdMeshH5Path) -> XrdMeshSource
mkXrdMeshSourceNxs f e h = XrdMeshSourceNxs (Nxs' f e (h e))

data XrdMeshFrame = XrdMeshFrame
                    WaveLength
                    (MyMatrix Double)
                  deriving (Show)

class FrameND t where
  rowND :: t -> MaybeT IO XrdMeshFrame

instance FrameND XrdMeshH5 where

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
      get_position' :: Shape sh => Dataset -> sh -> MaybeT IO (Vector Double)
      get_position' a b = lift $ do
        v <- get_position_new a b
        if any isNaN v then fail "File contains Nan" else return v


withDataSource :: File -> XrdMeshSource -> (XrdMeshH5 -> IO r) -> IO r
withDataSource h s = bracket (before s) after
    where
      before ::XrdMeshSource -> IO XrdMeshH5
      before (XrdMeshSourceNxs (Nxs' _ _ (XrdMeshH5Path _i x y g d w))) = XrdMeshH5
                                                                          <$> openDataset' h x
                                                                          <*> openDataset' h y
                                                                          <*> openDataset' h g
                                                                          <*> openDataset' h d
                                                                          <*> openDataset' h w


      after :: XrdMeshH5 -> IO ()
      after (XrdMeshH5 x' y' g' d' w') = do
        closeDataset x'
        closeDataset y'
        closeDataset g'
        closeDataset d'
        closeDataset w'

      openDataset' :: File -> DataItem -> IO Dataset
      openDataset' hid (DataItem name _) = openDataset hid (Char8.pack name) Nothing


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



integrateMesh :: PoniExt -> XrdMeshSample -> IO ()
integrateMesh ref (XrdMeshSample _ output nxss) =
  mapM_ (integrateMesh' ref output) nxss

integrateMesh' :: PoniExt -> OutputBaseDir -> XrdMesh -> IO ()
integrateMesh' ref output (XrdMesh b _ t nxs'@(XrdMeshSourceNxs (Nxs' f _ h5path))) =
  withH5File f $ \h5file ->
      withDataSource h5file nxs' $ \h -> do
        -- read the first frame and get the poni used for all the integration.
        d <- runMaybeT $ rowND h
        let (XrdMeshFrame w m) = fromJust d
        let (PoniExt p _) = setPose ref m

        -- save the poni at the right place.
        let sdir = (dropExtension . takeFileName) f
        let pfilename = output </> sdir </> sdir ++ ".poni"
        saveScript (poniToText p) pfilename

        -- create the python script to do the integration.
        let (XrdMeshH5Path (DataItem i _) (DataItem x _) (DataItem y _) _ _ _) = h5path
        let o = output </> sdir </> sdir ++ ".h5"
        let os = output </> sdir </> sdir ++ ".py"
        let (script, scriptPath) = xrdMeshPy pfilename f x y i b t w o os

        -- save the script
        saveScript script scriptPath

        -- run the script
        ExitSuccess <- runPythonScript scriptPath False

        return ()
