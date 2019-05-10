{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module Hkl.Projects.Sixs
    ( main_sixs )
        where

import Bindings.HDF5.Core (HSize)
import Bindings.HDF5.File ( AccFlags(Truncate), createFile)
import Bindings.HDF5.Dataset (createDataset, writeDataset)
import Bindings.HDF5.Datatype.Internal (nativeTypeOf)
import Bindings.HDF5.Dataspace (Dataspace, closeDataspace, createSimpleDataspace)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Array.Repa (Array)
import Data.Array.Repa.Index (DIM2)
import Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr)
import Data.ByteString.Char8 (pack)
import Data.Vector.Storable (concat, head, unsafeFromForeignPtr0)
import Data.Word (Word16)
import Numeric.LinearAlgebra (Matrix)
import Numeric.Units.Dimensional.Prelude (meter, nano, (*~))
import Pipes (Consumer, Producer, await, runEffect, (>->), yield)
import Pipes.Prelude (print, tee)
import Pipes.Safe (MonadSafe, SafeT, bracket, runSafeT)
import System.FilePath.Posix ((</>))

import Hkl ( DataItem ( DataItemH5 )
           , Dataset
           , Detector(..)
           , ExtendDims ( ExtendDims, StrictDims )
           , Factory(Uhv)
           , File
           , Geometry(Geometry)
           , H5
           , Source(Source)
           , closeDataset
           , closeFile
           , get_image
           , get_position
           , get_ub
           , lenH5Dataspace
           , openDataset
           , openH5
           )

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
    bracket (liftIO $ openH5 fp) (liftIO . closeFile) $ \f ->
    withDataset f i $ \i' ->
    withDataset f m $ \m' ->
    withDataset f o $ \o' ->
    withDataset f d $ \d' ->
    withDataset f g $ \g' ->
    withDataset f u $ \u' ->
    withDataset f w $ \w' ->
    withDataset f t $ \_t -> do
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
      withDataset :: (MonadSafe m) => File -> DataItem H5 -> (Dataset -> m r) -> m r
      withDataset hid (DataItemH5 name _) = bracket (liftIO $ openDataset hid (pack name) Nothing) (liftIO . closeDataset)

saveP :: FilePath -> DataItem H5 -> Consumer DataFrame (SafeT IO) ()
saveP f p = bracket acquire release $ \f' ->
            withNewDataspace [240, 560] $ \dataspace ->
            withNewDataset f' dataspace $ \dataset -> forever $ do
              (DataFrame j g ub image) <- await
              let fptr = toForeignPtr image
                  vector = unsafeFromForeignPtr0 fptr (240 * 560)
              liftIO $ writeDataset dataset Nothing Nothing Nothing vector
    where
      acquire = createFile (pack f) [Truncate] Nothing Nothing
      release = closeFile

      withNewDataspace :: MonadSafe m => [HSize] -> (Dataspace -> m r) -> m r
      withNewDataspace s = bracket
                           (liftIO $ createSimpleDataspace s)
                           (liftIO . closeDataspace)

      withNewDataset :: (MonadSafe m) => File -> Dataspace -> (Dataset -> m r) -> m r
      withNewDataset f' dataspace = bracket
                                    (liftIO $ createDataset f' (pack "imgs") (nativeTypeOf (0 :: Word16)) dataspace Nothing Nothing Nothing)
                                    (liftIO . closeDataset)

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

  runSafeT $ runEffect $
    framesP (root </> filename) dataframe_h5p ImXpadS140
    >-> Pipes.Prelude.tee  Pipes.Prelude.print
    >-> saveP "/tmp/test.h5" outPath
