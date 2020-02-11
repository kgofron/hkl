{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Sixs
  (process) where

import           Control.Monad                     (filterM, forM_, forever)
import           Control.Monad.Catch               (MonadThrow)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa.Index             (DIM1, DIM3, Z)
import           Data.Ini.Config                   (parseIniFile)
import           Data.List                         (isInfixOf)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (pack, replace, unpack)
import           Data.Text.IO                      (readFile)
import           Data.Vector.Storable              (concat, head)
import           Data.Word                         (Word16)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (degree, (*~))
import           Path                              (Abs, File, Path,
                                                    fileExtension, toFilePath)
import           Path.IO                           (listDir)
import           Pipes                             (await, yield)
import           Text.Printf                       (printf)

import           Prelude                           hiding (readFile)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.C.Geometry
import           Hkl.H5                            hiding (File)
import           Hkl.Pipes
import           Hkl.Types
import           Paths_hkl

data DataFrameQxQyQzInput
  = DataFrameQxQyQzInput
    (Hdf5Path DIM3 Word16) -- Image
    (Hdf5Path DIM1 Double) -- Mu
    (Hdf5Path DIM1 Double) -- Omega
    (Hdf5Path DIM1 Double) -- Delta
    (Hdf5Path DIM1 Double) -- Gamma
    (Hdf5Path Z Double) -- Wavelength
    (Hdf5Path DIM1 Char) -- DiffractometerType

instance Show DataFrameQxQyQzInput where
  show _ = ""

instance FramesP DataFrameQxQyQzInput where
  lenP (DataFrameQxQyQzInput _ m _ _ _ _ _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f m $ \m' -> do
      (Just n) <- liftIO $ lenH5Dataspace m'
      yield n

  framesP (DataFrameQxQyQzInput i m o d g w t) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' ->
      withHdf5PathP f m $ \m' ->
      withHdf5PathP f o $ \o' ->
      withHdf5PathP f d $ \d' ->
      withHdf5PathP f g $ \g' ->
      withHdf5PathP f w $ \w' ->
      withHdf5PathP f t $ \_t' ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                       (do
                           mu <- get_position m' j
                           omega <- get_position o' j
                           delta <- get_position d' j
                           gamma' <- get_position g' j
                           wavelength <- get_position w' 0
                           image <- get_image' det i' j
                           let positions = Data.Vector.Storable.concat [mu, omega, delta, gamma']
                               source = Source (Data.Vector.Storable.head wavelength *~ angstrom)
                           pure $ DataFrameQxQyQz j (Geometry Uhv source positions Nothing) image))


files :: BinocularsConfig -> IO [Path Abs File]
files c' = do
  (_, fs) <- listDir (nexusdir . input $ c')
  fs' <- filterM isHdf5 fs
  return $ case inputrange . input $ c' of
    Just r  -> filter (isInConfigRange r) fs'
    Nothing -> fs'
    where
      isHdf5 :: MonadThrow m => Path Abs File -> m Bool
      isHdf5 p = do
               let e = fileExtension p
               return  $ e `elem` [".h5", ".nxs"]

      matchIndex :: Path Abs File -> Int -> Bool
      matchIndex p n = printf "%05d" n `isInfixOf` toFilePath p

      isInConfigRange :: ConfigRange Int -> Path Abs File -> Bool
      isInConfigRange (ConfigRange []) _ = True
      isInConfigRange (ConfigRange [from]) p = any (matchIndex p) [from]
      isInConfigRange (ConfigRange [from, to]) p = any (matchIndex p) [from..to]
      isInConfigRange (ConfigRange (from:to:_)) p = any (matchIndex p) [from..to]

h5dpath' :: InputType -> DataFrameQxQyQzInput
h5dpath' SixsFlyScanUhv = DataFrameQxQyQzInput
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                          -- (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1/" $ datasetp "UB")
                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "type")
h5dpath' SixsFlyScanUhv2 = DataFrameQxQyQzInput
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                           -- (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx2-ex-diff-uhv" $ datasetp "UB")
                           (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                           (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx2-ex-diff-uhv" $ datasetp "type")

replace' :: Int -> Int -> DestinationTmpl -> FilePath
replace' f t = unpack . replace "{last}" (pack . show $ t) . replace "{first}" (pack . show $ f) . unDestinationTmpl

destination' :: ConfigRange Int -> DestinationTmpl -> FilePath
destination' (ConfigRange [])          = replace' 0 0
destination' (ConfigRange [from])      = replace' from from
destination' (ConfigRange [from, to])  = replace' from to
destination' (ConfigRange (from:to:_)) = replace' from to

mkInput :: BinocularsConfig -> IO (Input DataFrameQxQyQzInput)
mkInput c' = do
  fs <- files c'
  pure $ Input { filename = InputList fs
               , h5dpath = h5dpath' (itype . input $ c')
               , output = case inputrange . input $ c' of
                            Just r  -> destination' r (destination . dispatcher $ c')
                            Nothing -> destination' (ConfigRange []) (destination . dispatcher $ c')
               , resolutions = resolution . projection $ c'
               , centralPixel = centralpixel . input $ c'
               , sdd' = sdd . input $ c'
               , detrot' = fromMaybe (0 *~ degree) (detrot . input $ c')
               }

process :: Maybe FilePath -> IO ()
process mf = do
  cfg <- readFile =<< case mf of
                       Nothing -> getDataFileName "data/test/config_manip1.cfg"
                       (Just f) -> pure f
  let r = parseIniFile cfg parseBinocularsConfig
  case r of
    Right c' -> do
      print c'
      i <- mkInput c'
      print i
      process' i
      return ()
    Left e   -> print e
