{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Hkl.Xrd.Calibration
       ( NptExt(..)
       , XRDCalibrationEntry(..)
       , XRDCalibration(..)
       , calibrate
       , extractEdf
       ) where

import           Control.Monad.IO.Class            (liftIO)
import           Data.Array.Repa.Index             (DIM2)
import           Data.ByteString.Char8             (pack)
import           Data.List                         (foldl')
import           Data.Text                         (pack, unlines)
import           Data.Vector.Storable              (Vector, fromList, slice,
                                                    toList)
import           Numeric.GSL.Minimization          (MinimizeMethod (NMSimplex2),
                                                    minimizeV)
import           Numeric.LinearAlgebra             (Matrix, atIndex, ident,
                                                    ( #> ), (<>))
import           Numeric.Units.Dimensional.Prelude (meter, nano, radian, (*~),
                                                    (/~))
import           Pipes.Safe                        (MonadSafe, bracket,
                                                    runSafeT)
import           System.Exit                       (ExitCode (ExitSuccess))
import           System.FilePath.Posix             (takeFileName, (</>))
import           Text.Printf                       (printf)

import           Prelude                           hiding ((<>))

import           Hkl.C
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Edf
import           Hkl.H5
import           Hkl.MyMatrix
import           Hkl.Nxs
import           Hkl.PyFAI
import           Hkl.Python
import           Hkl.Script
import           Hkl.Types
import           Hkl.Xrd.OneD

--  Calibration

data NptExt a = NptExt { nptExtNpt      :: Npt
                       , nptExtPose     :: Pose
                       , nptExtDetector :: Detector a DIM2
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

data XRDCalibration a = XRDCalibration { xrdCalibrationName :: SampleName
                                       , xrdCalibrationOutputDir :: AbsDirPath
                                       , xrdCalibrationDetector ∷ Detector a DIM2
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

getPoseNxs :: File -> DataFrameH5Path XrdOneD -> Int -> IO Pose -- TODO move to XRD
getPoseNxs f (XrdOneDH5Path _ g d w) i' = runSafeT $
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
      let source = Source (wavelength *~ nano meter)
      let positions = Data.Vector.Storable.fromList [mu, komega, kappa, kphi, gamma, delta]
      let geometry = Geometry K6c source positions Nothing
      let detector = ZeroD
      m <- geometryDetectorRotationGet geometry detector
      return $ Pose (MyMatrix HklB m)


getWavelength ∷ File → DataFrameH5Path XrdOneD → IO WaveLength
getWavelength f (XrdOneDH5Path _ _ _ w) = runSafeT $
    withDataItem f w $ \w' -> liftIO $ do
      wavelength <- get_position w' 0
      return $ wavelength *~ nano meter

readWavelength :: XRDCalibrationEntry -> IO WaveLength
readWavelength e =
    withH5File f $ \h5file -> getWavelength h5file p
    where
      (Nxs f p) = xrdCalibrationEntryNxs'Nxs e


readXRDCalibrationEntry :: Detector a DIM2 -> XRDCalibrationEntry -> IO (NptExt a)
readXRDCalibrationEntry d e@XRDCalibrationEntryNxs{} =
  withH5File f $ \h5file -> NptExt
                            <$> nptFromFile (xrdCalibrationEntryNxs'NptPath e)
                            <*> getPoseNxs h5file p idx
                            <*> pure d
  where
    idx = xrdCalibrationEntryNxs'Idx e
    (Nxs f p) = xrdCalibrationEntryNxs'Nxs e
readXRDCalibrationEntry d e@(XRDCalibrationEntryEdf _ _) =
  NptExt
  <$> nptFromFile (xrdCalibrationEntryEdf'NptPath e)
  <*> getPoseEdf (xrdCalibrationEntryEdf'Edf e)
  <*> pure d

--  Poni Calibration

-- The minimized function is the quadratic difference of the
-- theoretical tth angle and for each pixel, the computed tth angle.

-- synonyme types use in order to improve the calibration performance

type NptEntry' = (Double, [Vector Double]) -- tth, detector pixels coordinates
type Npt' = (Double, [NptEntry']) -- wavelength, [NptEntry']
type NptExt' a = (Npt', Matrix Double, Detector a DIM2)

class ToGsl a where
  toGsl ∷ a → Vector Double

class FromGsl a where
  fromGsl ∷ a → Vector Double → a

class ToGslFunc a where
  toGslFunc ∷ a → [NptExt b] → (Vector Double → Double)

instance ToGsl PoniExt where
  toGsl (PoniExt p _) = fromList $ poniEntryToList (last p)

instance FromGsl PoniExt where
  fromGsl (PoniExt p pose) v = PoniExt poni pose
    where
      poni ∷ Poni
      poni = [poniEntryFromList (last p) (toList v)]

instance ToGslFunc PoniExt where
  toGslFunc _ npts = f (preCalibrate npts)
    where
      preCalibrate''' ∷ Detector a sh → NptEntry → NptEntry'
      preCalibrate''' detector (NptEntry _ tth _ points) = (tth /~ radian, map (coordinates detector) points)

      preCalibrate'' ∷ Npt → Detector a sh → Npt'
      preCalibrate'' n detector = (nptWavelength n /~ meter, map (preCalibrate''' detector) (nptEntries n))

      preCalibrate' ∷ NptExt a → NptExt' a
      preCalibrate' (NptExt n (Pose m) detector) = (preCalibrate'' n detector, m', detector)
        where
          (MyMatrix _ m') = changeBase m PyFAIB

      preCalibrate ∷ [NptExt a] → [NptExt' a]
      preCalibrate = map preCalibrate'

      f :: [NptExt' a] → Vector Double → Double
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

      f' ∷ Matrix Double → Vector Double → Double → NptExt' a → Double
      f' rotation translation x ((_wavelength, entries), m, _detector) =
        foldl' (f'' translation r) x entries
          where
            r :: Matrix Double
            r = m <> rotation

      f'' ∷ Vector Double → Matrix Double → Double → NptEntry' → Double
      {-# INLINE f'' #-}
      f'' translation r x (tth, pixels) = foldl' (f''' translation r tth) x pixels

      f''' ∷ Vector Double → Matrix Double → Double → Double → Vector Double → Double
      {-# INLINE f''' #-}
      f''' translation r tth x pixel = x + dtth * dtth
          where
            kf = r #> (pixel - translation)
            x' = kf `atIndex` 0
            y' = kf `atIndex` 1
            z' = kf `atIndex` 2

            dtth = tth - atan2 (sqrt (x'*x' + y'*y')) (-z')

calibrate ∷ XRDCalibration a → PoniExt → IO PoniExt
calibrate (XRDCalibration _ _ d _ es) p = do
  npts ← mapM (readXRDCalibrationEntry d) es
  let guess = toGsl p
  let f = toGslFunc p npts
  let box = fromList [0.1, 0.1, 0.1, 0.01, 0.01, 0.01]
  let (solution, _p) = minimizeV NMSimplex2 1E-16 3000 box f guess
  print _p
  return $ fromGsl p solution

--  Edf extraction before calibration

edf ∷ AbsDirPath → FilePath → Int → FilePath
edf o n i = o </> f
  where
    f = takeFileName n ++ printf "_%02d.edf" i

scriptExtractEdf ∷ AbsDirPath → [XRDCalibrationEntry] → Script Py2
scriptExtractEdf o es = Py2Script (content, scriptPath)
  where
    content = Data.Text.unlines $
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

scriptPyFAICalib ∷ AbsDirPath → XRDCalibrationEntry → Detector a sh → Calibrant → WaveLength → Script Sh
scriptPyFAICalib o e d c w = ScriptSh (content, scriptPath)
  where
    content = Data.Text.unlines $
              map Data.Text.pack [ "#!/usr/bin/env sh"
                                 , ""
                                 , "pyFAI-calib " ++ unwords args
                                 ]

    args = [ toPyFAICalibArg w
           , toPyFAICalibArg c
           , toPyFAICalibArg d
           , toPyFAICalibArg (edf o n i) ]

    (XRDCalibrationEntryNxs (Nxs n _) i _) = e

    scriptPath ∷ FilePath
    scriptPath = o </> takeFileName n ++ printf "_%02d.sh" i


instance ExtractEdf (XRDCalibration a) where
  extractEdf (XRDCalibration _ o d c es) = do
    let script = scriptExtractEdf o es
    ExitSuccess ← run script False
    mapM_ go es
    where
      go e = do
        w ← readWavelength e
        scriptSave $ scriptPyFAICalib o e d c w
