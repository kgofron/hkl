{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module Hkl.Projects.Sixs
       ( main_sixs )
       where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Data.ByteString.Char8 (pack)
import Data.Vector.Storable (concat, head)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Numeric.LinearAlgebra (Matrix)
import Numeric.Units.Dimensional.Prelude (meter, nano, (*~))
import Pipes (Producer, runEffect, (>->), lift, yield)
import Pipes.Prelude (print)
import System.FilePath.Posix ((</>))

import Hkl ( DataItem ( DataItemH5 )
           , Dataset
           , ExtendDims ( ExtendDims, StrictDims )
           , Factory(Uhv)
           , File
           , Geometry(Geometry)
           , H5
           , Source(Source)
           , check_ndims
           , closeDataset
           , get_position
           , get_ub
           , lenH5Dataspace
           , openDataset
           , withH5File
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

data DataFrameHklH5
    = DataFrameHklH5
      Dataset -- image
      Dataset -- mu
      Dataset -- omega
      Dataset -- delta
      Dataset -- gamma
      Dataset -- ub
      Dataset -- wavelength
      Dataset -- dtype

data DataFrame
    = DataFrame
      Int -- n
      Geometry -- geometry
      (Matrix Double) -- ub
    deriving (Show)

withDataframeH5 :: File -> DataFrameHklH5Path -> (DataFrameHklH5 -> IO r) -> IO r
withDataframeH5 h5file dfp = bracket (hkl_h5_open h5file dfp) hkl_h5_close

hkl_h5_open :: File -> DataFrameHklH5Path -> IO DataFrameHklH5
hkl_h5_open h5file (DataFrameHklH5Path i m o d g u w t) = DataFrameHklH5
                         <$> openDataset' h5file i
                         <*> openDataset' h5file m
                         <*> openDataset' h5file o
                         <*> openDataset' h5file d
                         <*> openDataset' h5file g
                         <*> openDataset' h5file u
                         <*> openDataset' h5file w
                         <*> openDataset' h5file t
  where
    openDataset' :: File -> DataItem H5 -> IO Dataset
    openDataset' hid (DataItemH5 name _) = openDataset hid (pack name) Nothing

hkl_h5_is_valid :: DataFrameHklH5 -> IO Bool
hkl_h5_is_valid (DataFrameHklH5 _ m o d g _ _ _) = do
  True <- check_ndims m 1
  True <- check_ndims o 1
  True <- check_ndims d 1
  True <- check_ndims g 1
  return True

hkl_h5_close :: DataFrameHklH5 -> IO ()
hkl_h5_close (DataFrameHklH5 i m o d g u w t) = do
  closeDataset i
  closeDataset m
  closeDataset o
  closeDataset d
  closeDataset g
  closeDataset u
  closeDataset w
  closeDataset t

getDataFrame' ::  DataFrameHklH5 -> Int -> IO DataFrame
getDataFrame' (DataFrameHklH5 _ m o d g u w _) i = do
  mu <- get_position m i
  omega <- get_position o i
  delta <- get_position d i
  gamma <- get_position g i
  wavelength <- get_position w 0
  ub <- get_ub u
  let positions = Data.Vector.Storable.concat [mu, omega, delta, gamma]
  let source = Source (Data.Vector.Storable.head wavelength *~ nano meter)
  return $ DataFrame i (Geometry Uhv source positions Nothing) ub

getDataFrame :: DataFrameHklH5 -> Producer DataFrame IO ()
getDataFrame d@(DataFrameHklH5 _ m _ _ _ _ _ _) = do
  (Just n) <- lift $ lenH5Dataspace m
  forM_ [0..n-1] (\i -> lift (getDataFrame' d i) >>= yield)

main_sixs :: IO ()
main_sixs = do
  let root = "/nfs/ruche-sixs/sixs-soleil/com-sixs/2015/Shutdown4-5/XpadAu111/"
  let filename = "align_FLY2_omega_00045.nxs"
  let dataframe_h5p = DataFrameHklH5Path
                      (DataItemH5 "com_113934/scan_data/xpad_image" StrictDims)
                      (DataItemH5 "com_113934/scan_data/UHV_MU" ExtendDims)
                      (DataItemH5 "com_113934/scan_data/UHV_OMEGA" ExtendDims)
                      (DataItemH5 "com_113934/scan_data/UHV_DELTA" ExtendDims)
                      (DataItemH5 "com_113934/scan_data/UHV_GAMMA" ExtendDims)
                      (DataItemH5 "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/UB" StrictDims)
                      (DataItemH5 "com_113934/SIXS/Monochromator/wavelength" StrictDims)
                      (DataItemH5 "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/type" StrictDims)

  withH5File (root </> filename) $ \h5file ->
    withDataframeH5 h5file dataframe_h5p $ \dataframe_h5 -> do
      True <- hkl_h5_is_valid dataframe_h5
      runEffect $ getDataFrame dataframe_h5
        >->  Pipes.Prelude.print
