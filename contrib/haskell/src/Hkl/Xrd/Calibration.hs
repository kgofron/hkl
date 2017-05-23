{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Xrd.Calibration
       ( NptExt(..)
       , XRDCalibrationEntry(..)
       , XRDCalibration(..)
       , calibrate
       , extractEdf
       ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Data.List (foldl', intercalate)
import Data.Text (Text, unlines, pack)
import Data.Vector.Storable
  ( Vector
  , head
  , concat
  , fromList
  , slice
  , toList
  )
import Numeric.LinearAlgebra
  ( Matrix
  , (<>)
  , atIndex
  , ident
  )
import Numeric.GSL.Minimization
  ( MinimizeMethod(NMSimplex2)
  , minimizeV
  )
import Numeric.Units.Dimensional.Prelude (meter, radian, nano, (/~), (*~))
import Pipes.Safe (MonadSafe(..), runSafeT, bracket)
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath.Posix ((</>), takeFileName)
import Text.Printf ( printf )

import Prelude hiding (head, concat, lookup, readFile, writeFile, unlines)

import Hkl.C
import Hkl.DataSource
import Hkl.Detector
import Hkl.H5
import Hkl.PyFAI
import Hkl.Python
import Hkl.MyMatrix
import Hkl.Nxs
import Hkl.Script
import Hkl.Types
import Hkl.Xrd.OneD

#if !MIN_VERSION_hmatrix(0, 17, 0)
(#>) :: Matrix Double -> Vector Double -> Vector Double
(#>) = (<>)
#else
import Numeric.LinearAlgebra ((#>))
#endif

-- | Calibration

data NptExt a = NptExt { nptExtNpt :: Npt
                       , nptExtMyMatrix :: MyMatrix Double
                       , nptExtDetector :: Detector a
                       }
              deriving (Show)

data XRDCalibrationEntry = XRDCalibrationEntryNxs { xrdCalibrationEntryNxs'Nxs :: Nxs XrdOneD
                                                  , xrdCalibrationEntryNxs'Idx :: Int
                                                  , xrdCalibrationEntryNxs'NptPath :: FilePath
                                                  }
                         | XRDCalibrationEntryEdf { xrdCalibrationEntryEdf'Edf :: FilePath
                                                  , xrdCalibrationEntryEdf'NptPath :: FilePath
                                                  }
                           deriving (Show)

data XRDCalibration a = XRDCalibration { xrdCalibrationName :: Text
                                       , xrdCalibrationOutputDir :: FilePath
                                       , xrdCalibrationDetector ∷ Detector a
                                       , xrdCalibrationCalibrant ∷ Calibrant
                                       , xrdCalibrationEntries :: [XRDCalibrationEntry]
                                       }
                      deriving (Show)

withDataItem :: MonadSafe m => File -> DataItem H5 -> (Dataset -> m r) -> m r
withDataItem hid (DataItemH5 name _) = bracket (liftIO acquire') (liftIO . release')
    where
      acquire' :: IO Dataset
      acquire' = openDataset hid (Data.ByteString.Char8.pack name) Nothing

      release' :: Dataset -> IO ()
      release' = closeDataset

getMNxs :: File -> DataFrameH5Path XrdOneD -> Int -> IO (MyMatrix Double) -- TODO move to XRD
getMNxs f (XrdOneDH5Path _ g d w) i' = runSafeT $
    withDataItem f g $ \g' ->
    withDataItem f d $ \d' ->
    withDataItem f w $ \w' -> liftIO $ do
      let mu = 0.0
      let komega = 0.0
      let kappa = 0.0
      let kphi = 0.0
      gamma <- get_position g' 0
      delta <- get_position d' i'
      wavelength <- get_position w' 0
      let source = Source (head wavelength *~ nano meter)
      let positions = concat [mu, komega, kappa, kphi, gamma, delta]
      let geometry = Geometry K6c source positions Nothing
      let detector = ZeroD
      m <- geometryDetectorRotationGet geometry detector
      return (MyMatrix HklB m)


getWavelength ∷ File → DataFrameH5Path XrdOneD → IO WaveLength
getWavelength f (XrdOneDH5Path _ _ _ w) = runSafeT $
    withDataItem f w $ \w' -> liftIO $ do
      wavelength <- get_position w' 0
      return $ head wavelength *~ nano meter

readWavelength :: XRDCalibrationEntry -> IO (WaveLength)
readWavelength e =
    withH5File f $ \h5file -> getWavelength h5file p
    where
      (Nxs f p) = xrdCalibrationEntryNxs'Nxs e


readXRDCalibrationEntry :: Detector a -> XRDCalibrationEntry -> IO (NptExt a)
readXRDCalibrationEntry d e@(XRDCalibrationEntryNxs _ _ _) =
    withH5File f $ \h5file -> do
      m <- getMNxs h5file p idx
      npt <- nptFromFile (xrdCalibrationEntryNxs'NptPath e)
      return (NptExt npt m d)
    where
      idx = xrdCalibrationEntryNxs'Idx e
      (Nxs f p) = xrdCalibrationEntryNxs'Nxs e
readXRDCalibrationEntry d e@(XRDCalibrationEntryEdf _ _) = do
  m <- getMEdf (xrdCalibrationEntryEdf'Edf e)
  npt <-  nptFromFile (xrdCalibrationEntryEdf'NptPath e)
  return (NptExt npt m d)

-- | Poni Calibration

-- The minimized function is the quadratic difference of the
-- theoretical tth angle and for each pixel, the computed tth angle.

-- synonyme types use in order to improve the calibration performance

type NptEntry' = (Double, [Vector Double]) -- tth, detector pixels coordinates
type Npt' = (Double, [NptEntry']) -- wavelength, [NptEntry']
type NptExt' a = (Npt', Matrix Double, Detector a)

calibrate :: XRDCalibration a -> PoniExt -> IO PoniExt
calibrate c (PoniExt p _) =  do
  let entry = last p
  let guess = fromList $ poniEntryToList entry
  -- read all the NptExt
  npts <- mapM (readXRDCalibrationEntry (xrdCalibrationDetector c)) (xrdCalibrationEntries c)
  -- in order to improve computation speed, pre-compute the pixel coodinates.

  let (solution, _p) = minimizeV NMSimplex2 1E-16 3000 box (f (preCalibrate npts)) guess
  -- mplot $ drop 3 (toColumns p)
  print _p
  return $ PoniExt [poniEntryFromList entry (toList solution)] (MyMatrix HklB (ident 3))
    where
      preCalibrate''' :: Detector a -> NptEntry -> NptEntry'
      preCalibrate''' detector (NptEntry _ tth _ points) = (tth /~ radian, map (coordinates detector) points)

      preCalibrate'' :: Npt -> Detector a -> Npt'
      preCalibrate'' n detector = (nptWavelength n /~ meter, map (preCalibrate''' detector) (nptEntries n))

      preCalibrate' :: NptExt a -> NptExt' a
      preCalibrate' (NptExt n m detector) = (preCalibrate'' n detector, m', detector)
        where
          (MyMatrix _ m') = changeBase m PyFAIB

      preCalibrate :: [NptExt a] -> [NptExt' a]
      preCalibrate = map preCalibrate'

      box :: Vector Double
      box = fromList [0.1, 0.1, 0.1, 0.01, 0.01, 0.01]

      f :: [NptExt' a] -> Vector Double -> Double
      f ns params = foldl' (f' rotation translation) 0 ns
        where
            rot1 = params `atIndex` 0
            rot2 = params `atIndex` 1
            rot3 = params `atIndex` 2

            rotations = map (uncurry fromAxisAndAngle)
                        [ (fromList [0, 0, 1], rot3 *~ radian)
                        , (fromList [0, 1, 0], rot2 *~ radian)
                        , (fromList [1, 0, 0], rot1 *~ radian)]

            rotation = foldl' (<>) (ident 3) rotations

            translation :: Vector Double
            translation = slice 3 3 params

      f' :: Matrix Double -> Vector Double -> Double -> NptExt' a -> Double
      f' rotation translation x ((_wavelength, entries), m, _detector) =
        foldl' (f'' translation r) x entries
          where
            r :: Matrix Double
            r = m <> rotation

      f'' :: Vector Double -> Matrix Double -> Double -> NptEntry'-> Double
      {-# INLINE f'' #-}
      f'' translation r x (tth, pixels) = foldl' (f''' translation r tth) x pixels

      f''' :: Vector Double -> Matrix Double -> Double -> Double -> Vector Double -> Double
      {-# INLINE f''' #-}
      f''' translation r tth x pixel = x + dtth * dtth
          where
            kf = r #> (pixel - translation)
            x' = kf `atIndex` 0
            y' = kf `atIndex` 1
            z' = kf `atIndex` 2

            dtth = tth - atan2 (sqrt (x'*x' + y'*y')) (-z')




-- | Pre Calibration

edf ∷ FilePath → FilePath → Int → FilePath
edf o n i = o </> f
  where
    f = (takeFileName n) ++ printf "_%02d.edf" i

scriptExtractEdf ∷ FilePath → [XRDCalibrationEntry] → Script Py2
scriptExtractEdf o es = Py2Script (content, scriptPath)
  where
    content = unlines $
              map Data.Text.pack [ "#!/bin/env python"
                            , ""
                            , "from fabio.edfimage import edfimage"
                            , "from h5py import File"
                            , ""
                            , "NEXUSFILES = " ++ toPyVal nxss
                            , "IDXS = " ++ toPyVal idxs
                            , "IMAGEPATHS = " ++ toPyVal (imgs ∷ [String])
                            , "OUTPUTS = " ++ toPyVal outputs
                            , ""
                            , "for filename, i, p, o in zip(NEXUSFILES, IDXS, IMAGEPATHS, OUTPUTS):"
                            , "    with File(filename, mode='r') as f:"
                            , "        edfimage(f[p][i]).write(o)"
                            ]

    (nxss, idxs, imgs) = unzip3 [(f, i, img) | (XRDCalibrationEntryNxs (Nxs f (XrdOneDH5Path (DataItemH5 img _) _ _ _)) i _) ← es]

    outputs ∷ [FilePath]
    outputs = zipWith (edf o) nxss idxs

    scriptPath ∷ FilePath
    scriptPath = o </> "pre-calibration.py"

scriptPyFAICalib ∷ FilePath → XRDCalibrationEntry → Detector a → Calibrant → WaveLength → Script Sh
scriptPyFAICalib o e d c w = ScriptSh (content, scriptPath)
  where
    content = unlines $
              map Data.Text.pack [ "#!/usr/bin/env sh"
                                 , ""
                                 , "pyFAI-calib" ++ intercalate " " args
                                 ]

    args = [ toPyFAICalibArg w
           , toPyFAICalibArg c
           , toPyFAICalibArg d
           , toPyFAICalibArg (edf o n i) ]

    (XRDCalibrationEntryNxs (Nxs n _) i _) = e

    scriptPath ∷ FilePath
    scriptPath = o </> (takeFileName n) ++ printf "_%02d.sh" i


extractEdf ∷ XRDCalibration a → IO ()
extractEdf (XRDCalibration _ o d c es) = do
  let script = scriptExtractEdf o es
  ExitSuccess ← run script False
  mapM_ go es
  return ()
  where
    go e = do
      w ← readWavelength e
      scriptSave $ scriptPyFAICalib o e d c w
