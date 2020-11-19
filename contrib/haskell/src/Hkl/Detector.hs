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
       , Hkl
       , PyFAI
       , SomeDetector(..)
       , ZeroD
       , coordinates
       , defaultDetector
       , getDetectorDefaultMask
       , getPixelsCoordinates
       , parseDetector2D
       , shape
       ) where

import           Data.Array.Repa                   (Array)
import           Data.Array.Repa.Index             ((:.) (..), DIM0, DIM2, DIM3,
                                                    Z (..), ix2, ix3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, fromForeignPtr)
import           Data.List                         (find, sort)
import           Data.Text                         (Text, unpack)
import           Data.Vector.Storable              (Vector, fromList)
import           Data.Word                         (Word8)
import           Foreign.C.String                  (CString, peekCString)
import           Foreign.C.Types                   (CBool, CDouble (..),
                                                    CInt (..))
import           Foreign.ForeignPtr                (castForeignPtr,
                                                    newForeignPtr)
import           Foreign.Marshal.Alloc             (alloca, finalizerFree)
import           Foreign.Ptr                       (Ptr)
import           Foreign.Storable                  (peek)
import           GHC.IO.Unsafe                     (unsafePerformIO)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, meter,
                                                    radian, (/~))

import           Hkl.PyFAI.Npt                     (NptPoint (NptPoint))

data PyFAI deriving (Eq, Show)
data Hkl deriving (Eq, Show)
data ZeroD deriving (Eq, Show)

data Detector a sh where
  ImXpadS140 :: Detector PyFAI DIM2
  Xpad32 :: Detector PyFAI DIM2
  XpadFlatCorrected :: Detector PyFAI DIM2
  ZeroD :: Detector ZeroD DIM0
  Detector2D :: CInt -> String -> DIM2 -> Detector Hkl DIM2

deriving instance Show (Detector a sh)
deriving instance Eq (Detector a sh)

detectors :: [Detector Hkl DIM2]
detectors = unsafePerformIO $ do
  nd <- hkl_binoculars_detector_2d_number_of_detectors
  mapM mkDetector [0..nd-1]
       where
         mkDetector :: CInt -> IO (Detector Hkl DIM2)
         mkDetector n = Detector2D
                        <$> pure n
                        <*> (peekCString =<< hkl_binoculars_detector_2d_name_get n)
                        <*> (alloca $ \width ->
                                 alloca $ \height -> do
                                   hkl_binoculars_detector_2d_shape_get n width height
                                   w <- peek width
                                   h <- peek height
                                   return $ ix2 (fromEnum h) (fromEnum w))

defaultDetector ::Detector Hkl DIM2
defaultDetector = head detectors

foreign import ccall unsafe
  "hkl-binoculars.h hkl_binoculars_detector_2d_number_of_detectors"
  hkl_binoculars_detector_2d_number_of_detectors :: IO CInt

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_name_get"
 hkl_binoculars_detector_2d_name_get :: CInt -> IO CString

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_shape_get"
 hkl_binoculars_detector_2d_shape_get :: CInt -- HklBinocularsDetector2DEnum n
                                      -> Ptr CInt -- int *width
                                      -> Ptr CInt -- int *height
                                      -> IO ()

parseDetector2D :: Text -> Either String (Detector Hkl DIM2)
parseDetector2D t = case find (\(Detector2D _ n _) -> n == (unpack t)) detectors of
                      (Just d) -> Right d
                      Nothing  -> Left ("Unsupported '" ++ unpack t ++ "' detector, select one of -> " ++ unwords (sort [n | (Detector2D _ n _) <- detectors]))

--  SomeDetector

data SomeDetector = forall a sh. SomeDetector (Detector a sh)

instance Show SomeDetector where
  show (SomeDetector v) = show v

instance Eq SomeDetector where
    (==) a b = show a == show b

shape :: Detector a sh -> sh
shape ImXpadS140         = ix2 240 560 -- y x
shape Xpad32             = ix2 960 560
shape XpadFlatCorrected  = ix2 1154 576
shape ZeroD              = Z
shape (Detector2D _ _ s) = s

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

getPixelsCoordinates :: Detector Hkl DIM2 -> (Int, Int) -> Length Double -> Angle Double -> IO (Array F DIM3 Double)
getPixelsCoordinates (Detector2D n _ sh) (ix0, iy0) sdd detrot = do
  parr <- hkl_binoculars_detector_2d_coordinates_get n
  let Z :. height :. width = sh
  hkl_binoculars_detector_2d_sixs_calibration n parr (toEnum width) (toEnum height) (toEnum ix0) (toEnum iy0) (CDouble (sdd /~ meter)) (CDouble (detrot /~ radian))
  arr <- newForeignPtr finalizerFree parr
  return $ fromForeignPtr (ix3 3 height width) (castForeignPtr arr)

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_coordinates_get"
 hkl_binoculars_detector_2d_coordinates_get :: CInt -- HklBinocularsDetector2DEnum n
                                            -> IO (Ptr CDouble)

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_sixs_calibration"
 hkl_binoculars_detector_2d_sixs_calibration :: CInt -- HklBinocularsDetector2DEnum n
                                             -> Ptr (CDouble) -- double *arr
                                             -> CInt -- int width
                                             -> CInt -- int height
                                             -> CInt -- int ix0
                                             -> CInt  -- int iy0
                                             -> CDouble -- double sdd
                                             -> CDouble -- double detrot
                                             -> IO ()

getDetectorDefaultMask :: Detector a DIM2 -> Maybe Text -> IO (Array F DIM2 Word8)
getDetectorDefaultMask (Detector2D n _ sh)  _ = do
  arr <- hkl_binoculars_detector_2d_mask_get n >>= newForeignPtr finalizerFree
  return $ fromForeignPtr sh (castForeignPtr arr)

foreign import ccall unsafe
 "hkl-binoculars.h hkl_binoculars_detector_2d_mask_get"
 hkl_binoculars_detector_2d_mask_get :: CInt -- HklBinocularsDetector2DEnum n
                                     -> IO (Ptr CBool)
