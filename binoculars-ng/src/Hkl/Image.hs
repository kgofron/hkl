{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Image
    ( Image(..)
    , filterSumImage
    , readImgInBuffer
    ) where

import           Data.Int                     (Int32)
import           Data.Vector.Storable.Mutable (IOVector, Storable, length,
                                               unsafeRead, unsafeWith)
import           Data.Word                    (Word16, Word32)
import           Foreign.C.String             (withCString)
import           System.IO.Unsafe             (unsafePerformIO)

import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.Repa

data Image = ImageInt32 (IOVector Int32)
           | ImageWord16 (IOVector Word16)
           | ImageWord32 (IOVector Word32)

instance Show Image where
  show (ImageInt32 _)  = "ImageInt32"
  show (ImageWord16 _) = "ImageWord16"
  show (ImageWord32 _) = "ImageWord32"

-- -- | /O(n)/ Monadic fold with strict accumulator (action applied to each element and its index).
-- --
-- -- @since 0.12.3.0
-- ifoldM' :: (PrimMonad m, Data.Vector.Generic.Mutable.MVector v a) => (b -> Int -> a -> m b) -> b -> v (PrimState m) a -> m b
-- {-# INLINE ifoldM' #-}
-- ifoldM' f b0 v = loop 0 b0
--   where
--     loop i !b | i >= n    = return b
--               | otherwise = do a <- unsafeRead v i
--                                loop (i + 1) =<< f b i a
--     n = length v
-- -- | /O(n)/ Pure left fold with strict accumulator (function applied to each element and its index).
-- --
-- -- @since 0.12.3.0
-- ifoldl' :: (PrimMonad m, Data.Vector.Generic.Mutable.MVector v a) => (b -> Int -> a -> b) -> b -> v (PrimState m) a -> m b
-- {-# INLINE ifoldl' #-}
-- ifoldl' f b0 v = stToPrim $ ifoldM' (\b i a -> return $ f b i a) b0 v

-- -- | /O(n)/ Pure left fold with strict accumulator.
-- --
-- -- @since 0.12.3.0
-- foldl' :: (PrimMonad m, Data.Vector.Generic.Mutable.MVector v a) => (b -> a -> b) -> b -> v (PrimState m) a -> m b
-- {-# INLINE foldl' #-}
-- foldl' f = ifoldl' (\b _ -> f b)
-- | /O(n)/ Monadic fold with strict accumulator (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM' :: Storable a => (b -> Int -> a -> IO b) -> b -> IOVector a -> IO b
{-# INLINE ifoldM' #-}
ifoldM' f b0 v = loop 0 b0
  where
    loop i !b | i >= n    = return b
              | otherwise = do a <- unsafeRead v i
                               loop (i + 1) =<< f b i a
    n = Data.Vector.Storable.Mutable.length v
-- | /O(n)/ Pure left fold with strict accumulator (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl' :: Storable a => (b -> Int -> a -> b) -> b -> IOVector a -> IO b
{-# INLINE ifoldl' #-}
ifoldl' f = ifoldM' (\b i a -> return $ f b i a)

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: Storable a => (b -> a -> b) -> b -> IOVector a -> IO b
{-# INLINE foldl' #-}
foldl' f = ifoldl' (\b _ -> f b)

sum' :: (Integral a, Storable a) => IOVector a -> Double
sum' v = unsafePerformIO $ foldl' (\b a -> b + fromIntegral a) 0.0 v

sumImage :: Image -> Double
sumImage (ImageInt32 i)  = sum' i
sumImage (ImageWord16 i) = sum' i
sumImage (ImageWord32 i) = sum' i


filterSumImage :: Maybe Double -> Image -> Bool
filterSumImage mMax img = case mMax of
                            Nothing  -> True
                            (Just m) -> sumImage img < m

readImgInBuffer :: IOVector Int32 -> (Detector Hkl DIM2) -> FilePath -> IO (IOVector Int32)
readImgInBuffer vec (Detector2D n _ _) fp
  = unsafeWith vec $ \buf ->
  withCString fp $ \c'fp -> do
  c'hkl_binoculars_detector_2d_img_load_into (toEnum . fromEnum $ n) c'fp buf ((toEnum $ Data.Vector.Storable.Mutable.length vec) * 4) -- TODO avoid this hardcode
  return vec
