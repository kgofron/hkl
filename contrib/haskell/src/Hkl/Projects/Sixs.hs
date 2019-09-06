{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Projects.Sixs
    ( main_sixs )
        where
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad                     (forM_)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, Shape, extent,
                                                    listOfShape, size)
import           Data.Array.Repa.Index             (DIM1, DIM2, DIM3, Z)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Vector.Storable              (concat, head)
import           Data.Word                         (Word16)
import           Foreign.C.Types                   (CInt (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr)
import           Foreign.Storable                  (peek)
import           Numeric.LinearAlgebra             (Matrix)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (meter, nano, (*~), (/~))
import           Pipes                             (Producer, yield)
import           Pipes.Prelude                     (toListM)
import           Pipes.Safe                        (SafeT, runSafeT)
import           System.FilePath.Posix             ((</>))

import           Hkl

data DataFrame
    = DataFrame
      Int -- n
      Geometry -- geometry
      (Matrix Double) -- ub
      (Array F DIM2 Word16) -- image

instance Show DataFrame where
  show (DataFrame i g m _) = unwords [show i, show g, show m]

class FramesP a where
  framesP :: FilePath -> a -> Detector b DIM2 -> Producer DataFrame (SafeT IO) ()

data DataFrameHklH5Path
  = DataFrameHklH5Path
    (Hdf5Path DIM3 Word16) -- Image
    (Hdf5Path DIM1 Double) -- Mu
    (Hdf5Path DIM1 Double) -- Omega
    (Hdf5Path DIM1 Double) -- Delta
    (Hdf5Path DIM1 Double) -- Gamma
    (Hdf5Path DIM2 Double) -- UB
    (Hdf5Path Z Double) -- Wavelength
    (Hdf5Path DIM1 Char) -- DiffractometerType

instance FramesP DataFrameHklH5Path where
  framesP fp (DataFrameHklH5Path i m o d g u w t) det =
    withFileP (openH5 fp) $ \f ->
    withHdf5PathP f i $ \i' ->
    withHdf5PathP f m $ \m' ->
    withHdf5PathP f o $ \o' ->
    withHdf5PathP f d $ \d' ->
    withHdf5PathP f g $ \g' ->
    withHdf5PathP f u $ \u' ->
    withHdf5PathP f w $ \w' ->
    withHdf5PathP f t $ \_t' -> do
      (Just n) <- liftIO $ lenH5Dataspace m'
      forM_ [0..n-1] (\j -> yield =<< liftIO
                       (do
                           mu <- get_position m' j
                           omega <- get_position o' j
                           delta <- get_position d' j
                           gamma <- get_position g' j
                           wavelength <- get_position w' 0
                           image <- get_image det i' j
                           ub <- get_ub u'
                           let positions = Data.Vector.Storable.concat [mu, omega, delta, gamma]
                               source = Source (Data.Vector.Storable.head wavelength *~ nano meter)
                           pure $ DataFrame j (Geometry Uhv source positions Nothing) ub image))

-- | DataFrameSpace

data DataFrameSpace sh = DataFrameSpace DataFrame (Space sh)
  deriving Show

space :: Detector a DIM2 -> Array F DIM3 Double -> DataFrame -> IO (DataFrameSpace DIM3)
space detector pixels df@(DataFrame _ g@(Geometry _ (Source w) _ _) _ub img) = do
  let resolutions = [0.0002, 0.0002, 0.0002] :: [Double]
  let k = 2 * pi / (w /~ angstrom)
  let nPixels = size . shape $ detector
  let pixelsDims = map toEnum $ listOfShape . extent $ pixels :: [CInt]
  withGeometry g $ \geometry ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen resolutions $ \nr r ->
    withArrayLen pixelsDims $ \ndim dims ->
    withForeignPtr (toForeignPtr img) $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_q geometry k i (toEnum nPixels) pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace df s)

-- | Create the Cube

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)

mkCube :: Shape sh => [DataFrameSpace sh] -> IO (Cube sh)
mkCube dfs = do
  let spaces = [spaceHklPointer s | (DataFrameSpace _ s) <- dfs]
  let images = [toForeignPtr img | (DataFrameSpace (DataFrame _ _ _ img) _) <- dfs]
  let (DataFrameSpace (DataFrame _ _ _ img) _ )= Prelude.head dfs
  let nPixels = size . extent $ img
  withForeignPtrs spaces $ \pspaces ->
    withForeignPtrs images $ \pimages ->
    withArrayLen pspaces $ \nSpaces' spaces' ->
    withArrayLen pimages $ \_ images' -> do
    peek =<< {-# SCC "hkl_binoculars_cube_new" #-} hkl_binoculars_cube_new (toEnum nSpaces') spaces' (toEnum nPixels) images'

main_sixs :: IO ()
main_sixs = do
  let root = "/nfs/ruche-sixs/sixs-soleil/com-sixs/2015/Shutdown4-5/XpadAu111/"
  -- let root = "/home/picca/"
  -- let root = "/home/experiences/instrumentation/picca/"
  let filename = "align_FLY2_omega_00045.nxs"
  let h5path = DataFrameHklH5Path
               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
               (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1/" $ datasetp "UB")
               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
               (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "type")

  let outputFilename = "test.hdf5"

  pixels <- getPixelsCoordinates ImXpadS140 0 0 1
  r <- runSafeT $ toListM $ framesP (root </> filename) h5path ImXpadS140
  r' <- mapConcurrently (space ImXpadS140 pixels) r
  c <- mkCube r'
  saveHdf5 outputFilename c
  print c

  return ()
