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
module Hkl.Binoculars.Common
  ( Input(..)
  , FramesP(..)
  , Chunk(..)
  , DataFrame(..)
  , InputFn(..)
  , process'
  ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Exception                 (bracket)
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, Shape, extent,
                                                    listOfShape, size)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.IORef                        (IORef, modifyIORef',
                                                    newIORef, readIORef)
import           Data.Word                         (Word16)
import           Foreign.C.Types                   (CInt (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr)
import           Foreign.Storable                  (peek)
import           GHC.Conc                          (getNumCapabilities)
import           Numeric.LinearAlgebra             (Matrix)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, (/~))
import           Pipes                             (Consumer, Pipe, await, each,
                                                    runEffect, (>->))
import           Pipes.Prelude                     (mapM, toListM)
import           Pipes.Safe                        (SafeT, runSafeT)
import           Text.Printf                       (printf)

import           Prelude                           hiding (mapM)

import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Types

data Chunk n a = Chunk !a !n !n
deriving instance (Show n, Show a) => Show (Chunk n a)

cweight :: Num n => Chunk n a -> n
cweight (Chunk _ l h) = h - l

csplit :: Num n => Chunk n a -> n -> (Chunk n a, Chunk n a)
csplit (Chunk a l h) n = (Chunk a l (l + n), Chunk a (l+n) h)

chunk :: (Num n, Ord n) => n -> [Chunk n a] -> [[Chunk n a]]
chunk target = go target target
  where
    go _ _ []          = []
    go tgt gap [x]     = golast tgt gap x
    go tgt gap ~(x:xs) =
      let gap' = gap - cweight x
      in if | gap' > 0                -> cons1 x $ go tgt gap' xs
            | gap' == 0                -> [x] : go tgt tgt xs
            | (x1, x2) <- csplit x gap -> [x1] : go tgt tgt (x2 : xs)

    cons1 x cs = (x : Prelude.head cs) : tail cs

    golast tgt gap x =
      if | cweight x <= gap         -> [[x]]
         | (x1, x2) <- csplit x gap -> [x1] : golast tgt tgt x2

{-# SPECIALIZE chunk :: Int -> [Chunk Int FilePath] -> [[Chunk Int FilePath]]  #-}

data DataFrame
    = DataFrame
      Int -- n
      Geometry -- geometry
      (Matrix Double) -- ub
      (ForeignPtr Word16) -- image

instance Show DataFrame where
  show (DataFrame i g m _) = unwords [show i, show g, show m]

class FramesP a where
  lenP :: a -> Pipe FilePath Int (SafeT IO) ()
  framesP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) DataFrame (SafeT IO) ()

-- | DataFrameSpace

data DataFrameSpace sh = DataFrameSpace (ForeignPtr Word16) (Space sh)
  deriving Show

type Resolutions = [Double]

{-# INLINE space #-}
space :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> DataFrame -> IO (DataFrameSpace DIM3)
space detector pixels rs (DataFrame _ g@(Geometry _ (Source w) _ _) _ub img) = do
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

spaceP :: (MonadIO m) => Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Pipe DataFrame (DataFrameSpace DIM3) m ()
spaceP detector pixels rs = mapM (liftIO . space detector pixels rs)

-- | Create the Cube

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)

{-# INLINE mkCube' #-}
mkCube' :: Shape sh => Detector a DIM2 -> [DataFrameSpace sh] -> IO (Cube' sh)
mkCube' detector dfs = do
  let spaces = [spaceHklPointer s | (DataFrameSpace _ s) <- dfs]
  let images = [img | (DataFrameSpace img _) <- dfs]
  let nPixels = size . shape $ detector
  withForeignPtrs spaces $ \pspaces ->
    withForeignPtrs images $ \pimages ->
    withArrayLen pspaces $ \nSpaces' spaces' ->
    withArrayLen pimages $ \_ images' ->
    peek =<< {-# SCC "hkl_binoculars_cube_new'" #-} hkl_binoculars_cube_new' (toEnum nSpaces') spaces' (toEnum nPixels) images'

mkCube'P :: (MonadIO m, Shape sh) => Detector a DIM2 -> IORef (Cube' sh) -> Consumer (DataFrameSpace sh) m ()
mkCube'P det ref = forever $ do
  s <- await
  c2 <- liftIO $ mkCube' det [s]
  liftIO $ modifyIORef' ref (c2 <>)

type Template = String

data InputFn = InputFn FilePath
             | InputRange Template Int Int


toList :: InputFn -> [FilePath]
toList (InputFn f)           = [f]
toList (InputRange tmpl f t) = [printf tmpl i | i <- [f..t]]

data Input a = Input { filename     :: InputFn
                     , h5dpath      :: a
                     , output       :: FilePath
                     , resolutions  :: [Double]
                     , centralPixel :: (Int, Int)  -- x, y
                     , sdd'         :: Length Float  -- sample to detector distance
                     , detrot'      :: Angle Float
                     }


mkJobs' :: Int -> [FilePath] -> [Int] -> [[Chunk Int FilePath]]
mkJobs' n fns ts = chunk n [Chunk f 0 t | (f, t) <- zip fns ts]

mkJobs :: FramesP a => Input a -> IO [[Chunk Int FilePath]]
mkJobs i = do
  let fns = toList $ filename i
  ns <- runSafeT $ toListM $
       each fns
       >-> lenP (h5dpath i)
  c <- getNumCapabilities
  let ntot = sum ns
  return $ mkJobs' (quot ntot c) fns ns

withCubeAccumulator :: Shape sh => (IORef (Cube' sh)  -> IO ()) -> IO (Cube' sh)
withCubeAccumulator f = bracket (newIORef EmptyCube') pure (\r -> f r >> readIORef r)

process' :: FramesP a => Input a -> IO ()
process' input = do
  let detector = ImXpadS140

  pixels <- getPixelsCoordinates detector (centralPixel input) (sdd' input) (detrot' input)

  jobs <- mkJobs input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesP (h5dpath input) detector
                           >-> spaceP detector pixels (resolutions input)
                           >-> mkCube'P detector s
                       ) jobs
  let c' = mconcat r'
  c <- toCube c'
  saveHdf5 (output input) c
  print c

  return ()
