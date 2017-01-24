{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

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
import Control.Concurrent.Async (mapConcurrently)
-- import Control.Error (EitherT, runEitherT, left, throwT)
import Control.Monad (forM_, forever, when, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Array.Repa (Shape, DIM1, DIM2, fromIndex, listOfShape, size, shapeOfList)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString.Char8 as Char8 (pack)
import qualified Data.List as List (intercalate, lookup)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text (unlines, pack, intercalate)
import Data.Text.IO (readFile)
import Data.Vector.Storable (Vector, any, concat, head)
import Numeric.LinearAlgebra (fromList)
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ((</>), dropExtension, takeFileName, takeDirectory)
import System.Process ( system )
import Text.Printf ( printf )

import Prelude hiding
    ( any
    , concat
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
import Pipes.Prelude (toListM, drain)
import Pipes.Safe ( MonadSafe(..), runSafeT, bracket )

import Hkl.C
import Hkl.Detector
import Hkl.Edf
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
import Hkl.Types
import Hkl.Utils
import Hkl.Xrd.OneD

-- | Types

type PoniGenerator' = Pose -> DIM2 -> IO PoniExt

data XrdMeshH5Path = XrdMeshH5Path
                     DataItem -- ^ Image
                     DataItem -- ^ meshx
                     DataItem -- ^ meshy
                     DataItem -- ^ gamma
                     DataItem -- ^ delta
                     DataItem -- ^ wavelength
                   deriving (Show)

data XrdMeshH5 = XrdMeshH5
                 XrdMeshSource -- ^ Nxs
                 Dataset -- ^ MeshX
                 Dataset -- ^ MeshY
                 Dataset -- ^ Gamma
                 Dataset -- ^ Delta
                 Dataset -- ^ Wavelength
                 PoniGenerator' -- ^ PoniGenerator'

data XrdMeshSample = XrdMeshSample SampleName OutputBaseDir [XrdMesh] -- ^ nxss

data XrdMesh = XrdMesh DIM1 DIM1 Threshold XrdMeshSource deriving (Show)

data XrdMeshSource = XrdMeshSourceNxs Nxs'
                   deriving (Show)

data Nxs' = Nxs' FilePath NxEntry XrdMeshH5Path
             deriving (Show)

mkXrdMeshSourceNxs :: FilePath -> NxEntry -> (NxEntry -> XrdMeshH5Path) -> XrdMeshSource
mkXrdMeshSourceNxs f e h = XrdMeshSourceNxs (Nxs' f e (h e))

data XrdMeshFrame = XrdMeshFrame
                    XrdMeshSource -- ^ source of the current frame
                    DIM2 -- ^ current index in the array
                    Bool -- ^ is it the eof of the stream
                    Geometry -- ^ diffractometer geometry
                    PoniExt -- ^ the ref poniext
                  deriving (Show)

class FrameND t where
  shapeND :: t -> MaybeT IO DIM2
  rowND :: t -> DIM2 -> MaybeT IO XrdMeshFrame

instance FrameND XrdMeshH5 where
  shapeND (XrdMeshH5 _ x y _ _ _ _) = do
    lx <- lift $ lenH5Dataspace x
    ly <- lift $ lenH5Dataspace y
    return $ shapeOfList [fromJust lx, fromJust ly]

  rowND d'@(XrdMeshH5 s _ _ g d w p) sh = do
    n <- shapeND d'
    let eof = size n == size sh
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- get_position' g sh
    delta <- get_position' d sh
    wavelength <- get_position' w sh
    let source = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    -- print positions
    let geometry =  Geometry K6c source positions Nothing
    let detector = ZeroD
    m <- lift $ geometryDetectorRotationGet geometry detector
    poniext <- lift $ p (MyMatrix HklB m) sh
    return $ XrdMeshFrame s sh eof geometry poniext
    where
      get_position' :: Shape sh => Dataset -> sh -> MaybeT IO (Vector Double)
      get_position' a b = lift $ do
        v <- get_position_new a b
        if any isNaN v then fail "File contains Nan" else return v

framesND :: Pipe XrdMeshH5 XrdMeshFrame IO ()
framesND = forever $ do
  d <- await
  sh <- lift $ runMaybeT $ shapeND d
  let n = size (fromJust sh)
  when (isJust sh)
    ( forM_ [0..n-1] (\i' -> do
                         f <- lift . runMaybeT $ rowND d (fromIndex (fromJust sh) i')
                         when (isJust f) (yield (fromJust f)))
    )

withDataSource :: MonadSafe m => File -> XrdMeshSource -> PoniGenerator' -> (XrdMeshH5 -> m r) -> m r
withDataSource h nxs'@(XrdMeshSourceNxs (Nxs' _ _ (XrdMeshH5Path _i x y g d w))) gen =
  bracket (liftIO before) (liftIO . after)
  where
    -- before :: File -> DataFrameH5Path -> m DataFrameH5
    before :: IO XrdMeshH5
    before =  XrdMeshH5
              <$> return nxs'
              <*> openDataset' h x
              <*> openDataset' h y
              <*> openDataset' h g
              <*> openDataset' h d
              <*> openDataset' h w
              <*> return gen

    after :: XrdMeshH5 -> IO ()
    after (XrdMeshH5 _n x' y' g' d' w' _p) = do
      closeDataset x'
      closeDataset y'
      closeDataset g'
      closeDataset d'
      closeDataset w'

    openDataset' :: File -> DataItem -> IO Dataset
    openDataset' hid (DataItem name _) = openDataset hid (Char8.pack name) Nothing

data XrdMeshFrame' = XrdMeshFrame' XrdMeshFrame FilePath

savePonies' :: (DIM2 -> FilePath) -> Pipe XrdMeshFrame XrdMeshFrame' IO ()
savePonies' g = forever $ do
  f@(XrdMeshFrame _ sh _ _ (PoniExt p _)) <- await
  let filename = g sh
  lift $ saveScript (poniToText p) filename
  yield $ XrdMeshFrame' f filename

integrateMesh :: PoniExt -> XrdMeshSample -> IO ()
integrateMesh ref (XrdMeshSample _ output nxss) =
  mapM_ (integrateMesh' ref output) nxss

integrateMesh' :: PoniExt -> OutputBaseDir -> XrdMesh -> IO ()
integrateMesh' ref output (XrdMesh _ _mb _t nxs'@(XrdMeshSourceNxs (Nxs' f _ _))) = do
  print f
  withH5File f $ \h5file ->
      runSafeT $ runEffect $
        withDataSource h5file nxs' (gen ref) yield
        >-> hoist lift (framesND
        --                >-> drain)
                        >-> savePonies' (pgen output f)
        --                >-> saveMultiGeometry mb t
                        >-> drain
                       )
  where
    gen :: PoniExt -> Pose -> DIM2 -> IO PoniExt
    gen ref' m _sh = return $ setPose ref' m

    pgen :: OutputBaseDir -> FilePath -> DIM2 -> FilePath
    pgen o nxs'' idx = o </> scandir </>  scandir ++ printf "_%02d" i ++ printf "_%02d.poni" j
      where
        [i, j] = listOfShape idx
        scandir = (dropExtension . takeFileName) nxs''
