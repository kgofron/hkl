{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Projections
  ( FramesQxQyQzP(..)
  , DataFrameQxQyQz(..)
  , InputQxQyQz(..)
  , processQxQyQz
  ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, extent, listOfShape,
                                                    size)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Word                         (Word16)
import           Foreign.C.Types                   (CInt (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Storable                  (peek)
import           GHC.Conc                          (getNumCapabilities)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, (/~))
import           Path                              (fromAbsFile)
import           Pipes                             (Pipe, each, runEffect,
                                                    (>->))
import           Pipes.Prelude                     (mapM, toListM)
import           Pipes.Safe                        (SafeT, runSafeT)
import           Text.Printf                       (printf)

import           Prelude                           hiding (mapM)

import           Hkl.Binoculars.Common
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Types

-- | QxQyQz Projection

type Resolutions = [Double]

data InputQxQyQz a = Input { filename     :: InputFn
                           , h5dpath      :: a
                           , output       :: FilePath
                           , resolutions  :: [Double]
                           , centralPixel :: (Int, Int)  -- x, y
                           , sdd'         :: Length Double  -- sample to detector distance
                           , detrot'      :: Angle Double
                           }
  deriving Show

data DataFrameQxQyQz
    = DataFrameQxQyQz
      Int -- n
      Geometry -- geometry
      (ForeignPtr Word16) -- image
    deriving Show

class FramesQxQyQzP a where
  lenP :: a -> Pipe FilePath Int (SafeT IO) ()
  framesQxQyQzP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) DataFrameQxQyQz (SafeT IO) ()

{-# INLINE spaceQxQyQz #-}
spaceQxQyQz :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> DataFrameQxQyQz -> IO (DataFrameSpace DIM3)
spaceQxQyQz detector pixels rs (DataFrameQxQyQz _ g@(Geometry _ (Source w) _ _) img) = do
  let k = 2 * pi / (w /~ angstrom)
  let nPixels = size . shape $ detector
  let pixelsDims = map toEnum $ listOfShape . extent $ pixels :: [CInt]
  withGeometry g $ \geometry ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withArrayLen pixelsDims $ \ndim dims ->
    withForeignPtr img $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_q geometry k i (toEnum nPixels) pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace img s)

toList :: InputFn -> [FilePath]
toList (InputFn f)           = [f]
toList (InputRange tmpl f t) = [printf tmpl i | i <- [f..t]]
toList (InputList fs)        = map fromAbsFile fs

mkJobs' :: Int -> [FilePath] -> [Int] -> [[Chunk Int FilePath]]
mkJobs' n fns ts = chunk n [Chunk f 0 t | (f, t) <- zip fns ts]

mkJobs :: FramesQxQyQzP a => InputQxQyQz a -> IO [[Chunk Int FilePath]]
mkJobs i = do
  let fns = toList $ filename i
  ns <- runSafeT $ toListM $
       each fns
       >-> lenP (h5dpath i)
  c <- getNumCapabilities
  let ntot = sum ns
  return $ mkJobs' (quot ntot c) fns ns

processQxQyQz :: FramesQxQyQzP a => InputQxQyQz a -> IO ()
processQxQyQz input = do
  let detector = ImXpadS140

  pixels <- getPixelsCoordinates detector (centralPixel input) (sdd' input) (detrot' input)

  jobs <- mkJobs input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesQxQyQzP (h5dpath input) detector
                           >-> mapM (liftIO . spaceQxQyQz detector pixels (resolutions input))
                           >-> mkCube'P detector s
                       ) jobs
  let c' = mconcat r'
  c <- toCube c'
  saveHdf5 (output input) c
  print c

  return ()
