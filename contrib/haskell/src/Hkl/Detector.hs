{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hkl.Detector
       ( Detector(..)
       , ImXpadS140
       , Xpad32
       , ZeroD
       , shape
       , coordinates
       , getPixelsCoordinates
       , getPixelsCoordinates'
       , toPyFAIDetectorName
       ) where

import Data.Array.Repa (Array)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa.Index (Z(..), DIM0, DIM2, DIM3, ix2)
import Data.Vector.Storable ( Vector
                            , fromList
                            )
import Safe.Partial (Partial)

import Hkl.PyFAI.Npt ( NptPoint ( NptPoint ) )
import Hkl.Python

data ImXpadS140
data Xpad32
data ZeroD

data Detector a sh where
  ImXpadS140 :: Detector ImXpadS140 DIM2
  Xpad32 :: Detector Xpad32 DIM2
  ZeroD :: Detector ZeroD DIM0

deriving instance Show (Detector a sh)

shape :: Detector a sh -> sh
shape ImXpadS140 = ix2 240 560 -- y x
shape Xpad32 = ix2 960 560
shape ZeroD = Z

-- | Xpad Family

type Gap = Double
type Width = Int
type Index = Int

-- an xpad line is like this (pixel size, index)
-- | s 0 | s 1 | s 2 | ... |   5/2 s (w - 1)  || 5/2 s w   | s (w + 1) | ...
xpadLine :: Width -> Index -> Double
xpadLine w i'
    | i' == 0        = s / 2
    | i' == 1        = s * 3 / 2
    | idx == 0       = s * (fromIntegral i' + 3 * fromIntegral c - 1 / 4)
    | idx <= (w - 2) = s * (fromIntegral i' + 3 * fromIntegral c + 1 / 2)
    | idx == (w - 1) = s * (fromIntegral i' + 3 * fromIntegral c + 5 / 4)
    | otherwise = error $ "wront coordinates" ++ show i'
    where
      s = 130e-6
      (c, idx) = divMod i' w

xpadLineWithGap :: Width -> Gap -> Index -> Double
xpadLineWithGap w g i' = s / 2 + (s * fromIntegral i') + g * fromIntegral (div i' w)
    where
      s = 130e-6

interp :: (Int -> Double) -> Double -> Double
interp f p
  | p0 == p1  = f p0
  | otherwise = (p - fromIntegral p0) * (f p1 - f p0) + f p0
  where
    p0 :: Int
    p0 = floor p

    p1 :: Int
    p1 = ceiling p

-- compute the coordinated at a given point

coordinates :: Detector a sh -> NptPoint -> Vector Double
coordinates ZeroD (NptPoint 0 0) = fromList [0, 0, 0]
coordinates ZeroD _ = error "No coordinates in a ZeroD detecteor"

coordinates ImXpadS140 (NptPoint x y) =
  fromList [ interp (xpadLine 120) y
           , interp (xpadLine  80) x
           , 0
           ]

coordinates Xpad32 (NptPoint x y) =
  fromList [ interp (xpadLineWithGap 120 3.57e-3) y
           , interp (xpadLine        80) x
           , 0]

getPixelsCoordinates' :: String -> Int -> Int -> Float -> IO (PyObject (Array F DIM3 Double))
getPixelsCoordinates' = defVVVVO [str|
from numpy import array, ascontiguousarray, ones
from pyFAI.detectors import ALL_DETECTORS

def export(name, ix0, iy0, sdd):
        # works only for flat detector.
        detector = ALL_DETECTORS[name]()
        y, x, _ = detector.calc_cartesian_positions()
        y0 = y[iy0, ix0]
        x0 = x[iy0, ix0]
        z = ones(x.shape) * -1 * sdd
        # return converted to the hkl library coordinates
        # x -> -y
        # y -> z
        # z -> -x
        return ascontiguousarray([-z, -(x - x0), (y - y0)])
|]

toPyFAIDetectorName :: Detector a DIM2 -> String
toPyFAIDetectorName ImXpadS140 = "imxpads140"
toPyFAIDetectorName Xpad32 = "xpad_flat"

getPixelsCoordinates :: Partial => Detector a DIM2 -> Int -> Int -> Float -> IO (Array F DIM3 Double)
getPixelsCoordinates d ix0 iy0 sdd = do
    p <- getPixelsCoordinates' (toPyFAIDetectorName d) ix0 iy0 sdd
    extractNumpyArray p
