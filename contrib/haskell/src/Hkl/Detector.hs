{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Hkl.Detector
       ( Detector(..)
       , Hkl
       , Mask
       , PyFAI
       , SomeDetector(..)
       , ZeroD
       , defaultDetector
       , getDetectorMask
       , getDetectorDefaultMask
       , getPixelsCoordinates
       , inDetector
       , newDetector
       , parseDetector2D
       , shape
       , withDetector
       ) where


import           Control.Monad                     ((<=<))
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    object, pairs, withObject,
                                                    (.:), (.=))
import           Data.Array.Repa                   (Array, Shape, inShape)
import           Data.Array.Repa.Index             (DIM0, DIM2, DIM3, Z (..),
                                                    ix2, ix3, (:.) (..))
import           Data.Array.Repa.Repr.ForeignPtr   (F, fromForeignPtr)
import           Data.List                         (find, sort)
import           Data.Text                         (Text, pack, unpack, unwords)
import           Foreign.C.String                  (peekCString, withCString)
import           Foreign.C.Types                   (CBool, CDouble (..))
import           Foreign.ForeignPtr                (ForeignPtr, castForeignPtr,
                                                    newForeignPtr,
                                                    withForeignPtr)
import           Foreign.Marshal.Alloc             (alloca, finalizerFree)
import           Foreign.Ptr                       (Ptr, nullPtr)
import           Foreign.Storable                  (peek)
import           GHC.IO.Unsafe                     (unsafePerformIO)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, meter,
                                                    radian, (/~))
import           Test.QuickCheck                   (Arbitrary (..), elements)


import           Hkl.C.Binoculars
import           Hkl.C.Hkl

data HklDetectorException = MaskShapeNotcompatible Text
                          | NoDefaultMask
    deriving (Show)
instance Exception HklDetectorException

data PyFAI deriving (Eq, Show)
data Hkl deriving (Eq, Show)
data ZeroD deriving (Eq, Show)

data Detector a sh where
  ImXpadS140 :: Detector PyFAI DIM2
  Xpad32 :: Detector PyFAI DIM2
  XpadFlatCorrected :: Detector PyFAI DIM2
  ZeroD :: Detector ZeroD DIM0
  Detector2D :: C'HklBinocularsDetectorEnum -> String -> DIM2 -> Detector Hkl DIM2

deriving instance Show (Detector a sh)
deriving instance Eq (Detector a sh)

instance FromJSON (Detector Hkl DIM2) where
    parseJSON = withObject "Detector2D" $ \v ->
                do n <- v .: "detector"
                   case parseDetector2D n of
                     Left e  -> fail e
                     Right d -> pure d

instance ToJSON (Detector Hkl DIM2) where
    -- this generates a Value
    toJSON (Detector2D _ name _) =
        object ["detector" .= name]

    -- this encodes directly to a bytestring Builder
    toEncoding (Detector2D _ name _) =
        pairs ("detector" .= name)

instance Arbitrary (Detector Hkl DIM2) where
  arbitrary = elements detectors

type Mask = Array F DIM2 CBool

-- Detector

withDetector :: Detector a sh -> (Ptr C'HklDetector -> IO b) -> IO b
withDetector d func = do
  fptr <- newDetector d
  withForeignPtr fptr func

newDetector :: Detector a sh -> IO (ForeignPtr C'HklDetector)
newDetector ZeroD = c'hkl_detector_factory_new 0 >>= newForeignPtr p'hkl_detector_free
newDetector _ = error "Can not use 2D detector with the hkl library"

{-# NOINLINE detectors #-}
detectors :: [Detector Hkl DIM2]
detectors = unsafePerformIO $ do
  nd <- c'hkl_binoculars_detector_2d_number_of_detectors
  mapM mkDetector [0..toEnum . fromEnum $ nd - 1]
       where
         mkDetector :: C'HklBinocularsDetectorEnum -> IO (Detector Hkl DIM2)
         mkDetector n = Detector2D n
                        <$> (peekCString =<< c'hkl_binoculars_detector_2d_name_get n)
                        <*> alloca (\width ->
                                 alloca $ \height -> do
                                   c'hkl_binoculars_detector_2d_shape_get n width height
                                   w <- peek width
                                   h <- peek height
                                   return $ ix2 (fromEnum h) (fromEnum w))

-- TODO s140 should be the default
defaultDetector :: Detector Hkl DIM2
defaultDetector = head detectors

parseDetector2D :: Text -> Either String (Detector Hkl DIM2)
parseDetector2D t = case find (\(Detector2D _ n _) -> n == unpack t) detectors of
                      (Just d) -> Right d
                      Nothing  -> Left ("Unsupported '" ++ unpack t ++ "' detector, select one of -> " ++ Prelude.unwords (sort [n | (Detector2D _ n _) <- detectors]))

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

getPixelsCoordinates :: Detector Hkl DIM2 -> (Int, Int) -> Length Double -> Angle Double -> IO (Array F DIM3 Double)
getPixelsCoordinates (Detector2D n _ sh) (ix0, iy0) sdd detrot = do
  parr <- c'hkl_binoculars_detector_2d_coordinates_get n
  let Z :. height :. width = sh
  c'hkl_binoculars_detector_2d_sixs_calibration n parr (toEnum width) (toEnum height) (toEnum ix0) (toEnum iy0) (CDouble (sdd /~ meter)) (CDouble (detrot /~ radian))
  arr <- newForeignPtr finalizerFree parr
  return $ fromForeignPtr (ix3 3 height width) (castForeignPtr arr)

fromPtr :: (MonadThrow m, MonadIO m, Shape sh) => sh -> HklDetectorException -> Ptr a -> m (Array F sh a)
fromPtr sh err ptr =
    if ptr == nullPtr
    then throwM err
    else do
      arr <- liftIO $ newForeignPtr finalizerFree ptr
      return $ fromForeignPtr sh (castForeignPtr arr)

getDetectorDefaultMask :: (MonadThrow m, MonadIO m) => Detector Hkl DIM2 -> m Mask
getDetectorDefaultMask (Detector2D n _ sh) =
    fromPtr sh NoDefaultMask =<< liftIO (c'hkl_binoculars_detector_2d_mask_get n)

getDetectorMask :: (MonadThrow m, MonadIO m) => Detector Hkl DIM2 -> Text -> m Mask
getDetectorMask (Detector2D n name sh)  mask = do
  let  err = MaskShapeNotcompatible (Data.Text.unwords [pack name, ": ", mask])
  liftIO $ withCString (unpack mask) $
         fromPtr sh err <=< c'hkl_binoculars_detector_2d_mask_load n

inDetector :: (Int, Int) -> Detector Hkl DIM2 -> Bool
inDetector (x, y) det = inShape (shape det) (ix2 y x)
