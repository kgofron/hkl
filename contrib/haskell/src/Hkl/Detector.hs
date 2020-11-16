{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Hkl.Detector
       ( Detector(..)
       , SomeDetector(..)
       , PyFAI
       , ZeroD
       , coordinates
       , getDetectorDefaultMask
       , getPixelsCoordinates
       , shape
       ) where

import           Data.Array.Repa                   (Array)
import           Data.Array.Repa.Index             (DIM0, DIM2, DIM3, Z (..),
                                                    ix2, ix3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, fromForeignPtr)
import           Data.Text                         (Text, unpack)
import           Data.Vector.Storable              (Vector, fromList)
import           Data.Word                         (Word8)
import           Foreign.C.Types                   (CBool, CDouble (..),
                                                    CInt (..))
import           Foreign.ForeignPtr                (castForeignPtr,
                                                    newForeignPtr)
import           Foreign.Marshal.Alloc             (finalizerFree)
import           Foreign.Ptr                       (Ptr)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, meter,
                                                    radian, (/~))

import           Hkl.PyFAI.Npt                     (NptPoint (NptPoint))
import           Hkl.Python

data PyFAI deriving (Eq, Show)
data ZeroD deriving (Eq, Show)

data Detector a sh where
  ImXpadS140 :: Detector PyFAI DIM2
  Xpad32 :: Detector PyFAI DIM2
  XpadFlatCorrected :: Detector PyFAI DIM2
  ZeroD :: Detector ZeroD DIM0

deriving instance Show (Detector a sh)
deriving instance Eq (Detector a sh)

--  SomeDetector

data SomeDetector = forall a sh. SomeDetector (Detector a sh)

instance Show SomeDetector where
  show (SomeDetector v) = show v

instance Eq SomeDetector where
    (==) a b = show a == show b

shape :: Detector a sh -> sh
shape ImXpadS140        = ix2 240 560 -- y x
shape Xpad32            = ix2 960 560
shape XpadFlatCorrected = ix2 1154 576
shape ZeroD             = Z

--  Xpad Family

type Gap = Double
type Width = Int
type Index = Int

-- an xpad line is like this (pixel size, index)
--  s 0 | s 1 | s 2 | ... |   5/2 s (w - 1)  || 5/2 s w   | s (w + 1) | ...
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

coordinates XpadFlatCorrected (NptPoint x y) =
    fromList [ y * 130e-6
             , x * 130e-6
             , 0]

toPyFAIDetectorName :: Detector a DIM2 -> String
toPyFAIDetectorName ImXpadS140        = "imxpads140"
toPyFAIDetectorName Xpad32            = "xpad_flat"
toPyFAIDetectorName XpadFlatCorrected = undefined

getPixelsCoordinates :: Detector a DIM2 -> (Int, Int) -> Length Double -> Angle Double -> IO (Array F DIM3 Double)
getPixelsCoordinates det (ix0, iy0) sdd detrot = do
  case det of
    XpadFlatCorrected -> do
             parr <- hkl_binoculars_detector_2d_coordinates_xpad_flat_corrected
             hkl_binoculars_detector_2d_sixs_calibration parr 576 1154 (toEnum ix0) (toEnum iy0) (CDouble (sdd /~ meter)) (CDouble (detrot /~ radian))
             arr <- newForeignPtr finalizerFree parr
             return $ fromForeignPtr (ix3 3 1154 576) (castForeignPtr arr)
    _ -> extractNumpyArray =<< pyfaiCoordinates (toPyFAIDetectorName det) ix0 iy0 (sdd /~ meter) (detrot /~ radian)
      where
        pyfaiCoordinates :: String -> Int -> Int -> Double -> Double -> IO (PyObject (Array F DIM3 Double))
        pyfaiCoordinates = defVVVVVO [str|
from math import cos, sin
from numpy import array, ascontiguousarray, copy, ones, tensordot
from pyFAI.detectors import ALL_DETECTORS

def M(theta, u):
    """
    :param theta: the axis value in radian
    :type theta: float
    :param u: the axis vector [x, y, z]
    :type u: [float, float, float]
    :return: the rotation matrix
    :rtype: numpy.ndarray (3, 3)
    """
    c = cos(theta)
    one_minus_c = 1 - c
    s = sin(theta)
    return array([[c + u[0]**2 * one_minus_c,
                   u[0] * u[1] * one_minus_c - u[2] * s,
                   u[0] * u[2] * one_minus_c + u[1] * s],
                  [u[0] * u[1] * one_minus_c + u[2] * s,
                   c + u[1]**2 * one_minus_c,
                   u[1] * u[2] * one_minus_c - u[0] * s],
                  [u[0] * u[2] * one_minus_c - u[1] * s,
                   u[1] * u[2] * one_minus_c + u[0] * s,
                   c + u[2]**2 * one_minus_c]])

def export(name, ix0, iy0, sdd, rot):
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
        pixels_ = array([-z, -(x - x0), (y - y0)])
        # rotate the detector in the hkl basis
        P = M(rot, [1, 0, 0])
        pixels = tensordot(P, pixels_, axes=1)
        return copy(ascontiguousarray(pixels))
|]

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_coordinates_xpad_flat_corrected"
 hkl_binoculars_detector_2d_coordinates_xpad_flat_corrected :: IO (Ptr CDouble)

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_sixs_calibration"
 hkl_binoculars_detector_2d_sixs_calibration :: Ptr (CDouble) -- double *arr
                                             -> CInt -- int width
                                             -> CInt -- int height
                                             -> CInt -- int ix0
                                             -> CInt  -- int iy0
                                             -> CDouble -- double sdd
                                             -> CDouble -- double detrot
                                             -> IO ()


getDetectorDefaultMask :: Detector a DIM2 -> Maybe Text -> IO (Array F DIM2 Word8)
getDetectorDefaultMask det mfn = do
  let fn = case mfn of
             (Just fn') -> unpack fn'
             Nothing    -> ""
  case det of
    XpadFlatCorrected -> do
             arr <- newForeignPtr finalizerFree
                 =<< hkl_binoculars_detector_2d_mask_xpad_flat_corrected
             return $ fromForeignPtr (ix2 1154 575) (castForeignPtr arr)
    _                 -> extractNumpyArray =<< pyfaiMask (toPyFAIDetectorName det) fn
      where
        pyfaiMask :: String -> String -> IO (PyObject (Array F DIM2 Word8))
        pyfaiMask = defVVO [str|
import fabio
from numpy import ascontiguousarray, bitwise_or, bool, copy, load
from pyFAI.detectors import ALL_DETECTORS

def export(name, fnmask):
    detector = ALL_DETECTORS[name]()
    mask = detector.mask
    if mask is not None:
        mask = mask.astype(bool)
    else:
        mask = zeros(detector.max_shape).astype(bool)
    if fnmask != "":
        mask = bitwise_or(mask,
                          fabio.open(fnmask).data)

    return copy(ascontiguousarray(mask))
|]

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_mask_xpad_flat_corrected"
 hkl_binoculars_detector_2d_mask_xpad_flat_corrected :: IO (Ptr CBool)
