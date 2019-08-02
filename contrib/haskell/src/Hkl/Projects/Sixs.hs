{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE QuasiQuotes              #-}
module Hkl.Projects.Sixs
    ( main_sixs )
        where

import           Bindings.HDF5.Dataset             (createDataset)
import           Bindings.HDF5.Dataspace           (createSimpleDataspace)
import           Bindings.HDF5.Datatype.Internal   (nativeTypeOf)
import           Bindings.HDF5.File                (AccFlags (Truncate),
                                                    createFile)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, copyS, extent,
                                                    listOfShape, size)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.ByteString.Char8             (pack)
import           Data.Vector.Storable              (concat, head)
import           Data.Word                         (Word16)
import           Foreign.ForeignPtr                (ForeignPtr, newForeignPtr,
                                                    withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (FunPtr, Ptr)
import           Numeric.LinearAlgebra             (Matrix)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (meter, nano, (*~), (/~))
import           Pipes                             (Consumer, Pipe, Producer,
                                                    await, cat, for, runEffect,
                                                    yield, (>->))
import           Prelude                           hiding (mapM)
-- import Pipes.Async ((>&>))
import           Pipes.Prelude                     (drain, print, tee)
import           Pipes.Safe                        (SafeT, runSafeT)
import           System.FilePath.Posix             ((</>))

import           Hkl

{-# ANN module "HLint: ignore Use camelCase" #-}

data DataFrameHklH5Path
    = DataFrameHklH5Path
      (DataItem H5) -- Image
      (DataItem H5) -- Mu
      (DataItem H5) -- Omega
      (DataItem H5) -- delta
      (DataItem H5) -- gamma
      (DataItem H5) -- UB
      (DataItem H5) -- Wavelength
      (DataItem H5) -- DiffractometerType
    deriving (Show)

data DataFrame
    = DataFrame
      Int -- n
      Geometry -- geometry
      (Matrix Double) -- ub
      (Array F DIM2 Word16) -- image

instance Show DataFrame where
  show (DataFrame i g m _) = show i ++ show g ++ show m

class FramesP a where
  framesP :: FilePath -> a -> Detector b DIM2 -> Producer DataFrame (SafeT IO) ()

instance FramesP DataFrameHklH5Path where
  framesP fp (DataFrameHklH5Path i m o d g u w t) det =
    withFileP (openH5 fp) $ \f ->
    withDatasetP (openDataset' f i) $ \i' ->
    withDatasetP (openDataset' f m) $ \m' ->
    withDatasetP (openDataset' f o) $ \o' ->
    withDatasetP (openDataset' f d) $ \d' ->
    withDatasetP (openDataset' f g) $ \g' ->
    withDatasetP (openDataset' f u) $ \u' ->
    withDatasetP (openDataset' f w) $ \w' ->
    withDatasetP (openDataset' f t) $ \_t -> do
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
        where
          openDataset' :: File -> DataItem H5 -> IO Dataset
          openDataset' hid (DataItemH5 name _) = openDataset hid (pack name) Nothing

-- | DataFrameQ

data DataFrameQ = DataFrameQ DataFrame (Array F DIM3 Double)

instance Show DataFrameQ where
    show (DataFrameQ df _) = show df

foreign import ccall safe "computeQ" c_computeQ :: Ptr Geometry -> Ptr Double -> Int -> Double -> IO ()

computeQ' :: Detector a DIM2 -> Array F DIM3 Double -> DataFrame -> IO DataFrameQ
computeQ' d pixels df@(DataFrame _ g@(Geometry _ (Source w) _ _) _ub _image) = do
  let k = 2 * pi / (w /~ angstrom)
  let arr = copyS pixels
  let nInOut = size . shape $ d
  withGeometry g $ \geometry ->
      withForeignPtr (toForeignPtr arr) $ \inOut -> do
          {-# SCC "c_computeQ" #-} c_computeQ geometry inOut nInOut k
          pure $ DataFrameQ df arr

computeQP :: MonadIO m =>  Detector a DIM2 -> Array F DIM3 Double -> Pipe DataFrame DataFrameQ m ()
computeQP d pixels = for cat $ \a -> do
                     b <- liftIO $ computeQ' d pixels a
                     yield b


-- | DataFrameSpace

data Space

data DataFrameSpace = DataFrameSpace DataFrameQ (ForeignPtr Space)
  deriving Show

foreign import ccall safe "fromImage" c_fromImage :: Ptr Double -> Int -> Ptr Double -> Ptr Int -> Int -> Ptr Word16 -> Ptr Int -> Int -> IO (Ptr Space)
foreign import ccall unsafe "&space_free" c_space_free :: FunPtr (Ptr Space -> IO ())

space :: MonadIO m => DataFrameQ -> m DataFrameSpace
space df@(DataFrameQ (DataFrame _ _ _ img) arr) = do
  let resolutions = [0.002, 0.002, 0.002]
  -- let labels = ["qx", "qy", "qz"]
  let larr = listOfShape . extent $ arr
  let limg = listOfShape . extent $ img
  p <- liftIO $ withArrayLen resolutions $ \rl r ->
    withForeignPtr (toForeignPtr arr) $ \c ->
    withArrayLen larr $ \cNDims cDims ->
    withForeignPtr (toForeignPtr img) $ \i ->
    withArrayLen limg $ \iNDims iDims ->
    {-# SCC "c_fromImage" #-} c_fromImage r rl c cDims cNDims i iDims iNDims
  fp <- liftIO $ newForeignPtr c_space_free p
  return (DataFrameSpace df fp)

spaceP :: MonadIO m => Pipe DataFrameQ DataFrameSpace m ()
spaceP = for cat $ \a -> do
  b <- liftIO $ space a
  yield b

saveP :: FilePath -> DataItem H5 -> Detector a DIM2 -> Consumer DataFrame (SafeT IO) ()
saveP f p det = withFileP (createFile (pack f) [Truncate] Nothing Nothing) $ \f' ->
            withDataspaceP (createSimpleDataspace [2009, 240, 560]) $ \dataspace ->
            withDatasetP (createDataset f' (pack "imgs") (nativeTypeOf (0 :: Word16)) dataspace Nothing Nothing Nothing) $ \dataset -> forever $ do
              (DataFrame j g ub image) <- await
              liftIO $ set_image det dataset dataspace j image

main_sixs :: IO ()
main_sixs = do
  let root = "/nfs/ruche-sixs/sixs-soleil/com-sixs/2015/Shutdown4-5/XpadAu111/"
      filename = "align_FLY2_omega_00045.nxs"
      dataframe_h5p = DataFrameHklH5Path
                      (DataItemH5 "com_113934/scan_data/xpad_image" StrictDims)
                      (DataItemH5 "com_113934/scan_data/UHV_MU" ExtendDims)
                      (DataItemH5 "com_113934/scan_data/UHV_OMEGA" ExtendDims)
                      (DataItemH5 "com_113934/scan_data/UHV_DELTA" ExtendDims)
                      (DataItemH5 "com_113934/scan_data/UHV_GAMMA" ExtendDims)
                      (DataItemH5 "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/UB" StrictDims)
                      (DataItemH5 "com_113934/SIXS/Monochromator/wavelength" StrictDims)
                      (DataItemH5 "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/type" StrictDims)
      outPath = (DataItemH5 "imgs" StrictDims)

  pixels <- getPixelsCoordinates ImXpadS140 0 0 1
  runSafeT $ runEffect $
    framesP (root </> filename) dataframe_h5p ImXpadS140
    >-> computeQP ImXpadS140 pixels
    >-> spaceP
    -- >-> Pipes.Prelude.tee  Pipes.Prelude.print
    >-> drain
    -- >-> saveP "/tmp/test.h5" outPath ImXpadS140
  return ()
