{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-
    Copyright  : Copyright (C) 2014-2024 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Repa
    ( Array
    , DIM0
    , DIM1
    , DIM2
    , DIM3
    , F
    , Z(..)
    , Shape
    , extent
    , fromForeignPtr
    , inShape
    , ix2
    , ix3
    , linearIndex
    , listOfShape
    , showShape
    , size
    , toForeignPtr
    , (:.)(..)
    ) where


-- import           Data.Array.Repa                   (Array, Shape, inShape)
-- import           Data.Array.Repa.Index             (DIM0, DIM2, DIM3, Z (..),
--                                                    ix2, ix3, (:.) (..))
-- import           Data.Array.Repa.Repr.ForeignPtr   (F, fromForeignPtr)

import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.Base           (quotInt, remInt)
import           System.IO.Unsafe

-- Source -----------------------------------------------------------------------
-- | Class of array representations that we can read elements from.
class Source r e where
 -- Arrays with a representation tag, shape, and element type.
 --   Use one of the type tags like `D`, `U` and so on for @r@,
 --   one of `DIM1`, `DIM2` ... for @sh@.
 data Array r sh e

 -- | O(1). Take the extent (size) of an array.
 extent :: Shape sh => Array r sh e -> sh

 -- | O(1). Shape polymorphic indexing.
 index, unsafeIndex
        :: Shape sh => Array r sh e -> sh -> e

 {-# INLINE index #-}
 index arr ix           = arr `linearIndex`       toIndex (extent arr) ix

 {-# INLINE unsafeIndex #-}
 unsafeIndex arr ix     = arr `unsafeLinearIndex` toIndex (extent arr) ix

 -- | O(1). Linear indexing into underlying, row-major, array representation.
 linearIndex, unsafeLinearIndex
        :: Shape sh => Array r sh e -> Int -> e

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex      = linearIndex

 -- | Ensure an array's data structure is fully evaluated.
 deepSeqArray
        :: Shape sh => Array r sh e -> b -> b


-- | Read elements from a foreign buffer.
instance Storable a => Source F a where
 data Array F sh a
        = AForeignPtr !sh !Int !(ForeignPtr a)

 linearIndex (AForeignPtr _ len fptr) ix
  | ix < len
        = unsafePerformIO
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr ix

  | otherwise
  = error "Repa: foreign array index out of bounds"
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (AForeignPtr _ _ fptr) ix
        = unsafePerformIO
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr ix
 {-# INLINE unsafeLinearIndex #-}

 extent (AForeignPtr sh _ _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (AForeignPtr sh len fptr) x
  = sh `deepSeq` len `seq` fptr `seq` x
 {-# INLINE deepSeqArray #-}

stage   = "Data.Array.Repa.Index"

data F

-- | An index of dimension zero
data Z  = Z
          deriving (Show, Read, Eq, Ord)

-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr
        :: Shape sh
        => sh -> ForeignPtr e -> Array F sh e
fromForeignPtr !sh !fptr
        = AForeignPtr sh (size sh) fptr
{-# INLINE fromForeignPtr #-}

-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array F sh e -> ForeignPtr e
toForeignPtr (AForeignPtr _ _ fptr)
        = fptr
{-# INLINE toForeignPtr #-}
class Eq sh => Shape sh where

        -- | Get the number of dimensions in a shape.
        rank    :: sh -> Int

        -- | The shape of an array of size zero, with a particular dimensionality.
        zeroDim :: sh

        -- | The shape of an array with size one, with a particular dimensionality.
        unitDim :: sh

        -- | Compute the intersection of two shapes.
        intersectDim :: sh -> sh -> sh

        -- | Add the coordinates of two shapes componentwise
        addDim  :: sh -> sh -> sh

        -- | Get the total number of elements in an array with this shape.
        size    :: sh -> Int

        -- | Check whether this shape is small enough so that its flat
        --      indices an be represented as `Int`. If this returns `False` then your
        --      array is too big. Mostly used for writing QuickCheck tests.
        sizeIsValid :: sh -> Bool


        -- | Convert an index into its equivalent flat, linear, row-major version.
        toIndex :: sh   -- ^ Shape of the array.
                -> sh   -- ^ Index into the array.
                -> Int

        -- | Inverse of `toIndex`.
        fromIndex
                :: sh   -- ^ Shape of the array.
                -> Int  -- ^ Index into linear representation.
                -> sh

        -- | Check whether an index is within a given shape.
        inShapeRange
                :: sh   -- ^ Start index for range.
                -> sh   -- ^ Final index for range.
                -> sh   -- ^ Index to check for.
                -> Bool

        -- | Convert a shape into its list of dimensions.
        listOfShape     :: sh -> [Int]

        -- | Convert a list of dimensions to a shape
        shapeOfList     :: [Int] -> sh

        -- | Ensure that a shape is completely evaluated.
        infixr 0 `deepSeq`
        deepSeq :: sh -> a -> a


-- | Check whether an index is a part of a given shape.
inShape :: forall sh
        .  Shape sh
        => sh           -- ^ Shape of the array.
        -> sh           -- ^ Index.
        -> Bool

{-# INLINE inShape #-}
inShape sh ix
        = inShapeRange zeroDim sh ix


-- | Nicely format a shape as a string
showShape :: Shape sh => sh -> String
showShape = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . listOfShape


ix2 :: Int -> Int -> DIM2
ix2 y x = Z :. y :. x
{-# INLINE ix2 #-}

ix3 :: Int -> Int -> Int -> DIM3
ix3 z y x = Z :. z :. y :. x
{-# INLINE ix3 #-}

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
        = !tail :. !head
        deriving (Show, Read, Eq, Ord)

type DIM0       = Z
type DIM1       = DIM0 :. Int
type DIM2       = DIM1 :. Int
type DIM3       = DIM2 :. Int

-- Shape ----------------------------------------------------------------------
instance Shape Z where
        {-# INLINE [1] rank #-}
        rank _                  = 0

        {-# INLINE [1] zeroDim #-}
        zeroDim                 = Z

        {-# INLINE [1] unitDim #-}
        unitDim                 = Z

        {-# INLINE [1] intersectDim #-}
        intersectDim _ _        = Z

        {-# INLINE [1] addDim #-}
        addDim _ _              = Z

        {-# INLINE [1] size #-}
        size _                  = 1

        {-# INLINE [1] sizeIsValid #-}
        sizeIsValid _           = True


        {-# INLINE [1] toIndex #-}
        toIndex _ _             = 0

        {-# INLINE [1] fromIndex #-}
        fromIndex _ _           = Z


        {-# INLINE [1] inShapeRange #-}
        inShapeRange Z Z Z      = True

        {-# NOINLINE listOfShape #-}
        listOfShape _           = []

        {-# NOINLINE shapeOfList #-}
        shapeOfList []          = Z
        shapeOfList _           = error $ stage ++ ".fromList: non-empty list when converting to Z."

        {-# INLINE deepSeq #-}
        deepSeq Z x             = x


instance Shape sh => Shape (sh :. Int) where
        {-# INLINE [1] rank #-}
        rank   (sh  :. _)
                = rank sh + 1

        {-# INLINE [1] zeroDim #-}
        zeroDim = zeroDim :. 0

        {-# INLINE [1] unitDim #-}
        unitDim = unitDim :. 1

        {-# INLINE [1] intersectDim #-}
        intersectDim (sh1 :. n1) (sh2 :. n2)
                = (intersectDim sh1 sh2 :. (min n1 n2))

        {-# INLINE [1] addDim #-}
        addDim (sh1 :. n1) (sh2 :. n2)
                = addDim sh1 sh2 :. (n1 + n2)

        {-# INLINE [1] size #-}
        size  (sh1 :. n)
                = size sh1 * n

        {-# INLINE [1] sizeIsValid #-}
        sizeIsValid (sh1 :. n)
                | size sh1 > 0
                = n <= maxBound `div` size sh1

                | otherwise
                = False

        {-# INLINE [1] toIndex #-}
        toIndex (sh1 :. sh2) (sh1' :. sh2')
                = toIndex sh1 sh1' * sh2 + sh2'

        {-# INLINE [1] fromIndex #-}
        fromIndex (ds :. d) n
                = fromIndex ds (n `quotInt` d) :. r
                where
                -- If we assume that the index is in range, there is no point
                -- in computing the remainder for the highest dimension since
                -- n < d must hold. This saves one remInt per element access which
                -- is quite a big deal.
                r       | rank ds == 0  = n
                        | otherwise     = n `remInt` d

        {-# INLINE [1] inShapeRange #-}
        inShapeRange (zs :. z) (sh1 :. n1) (sh2 :. n2)
                = (n2 >= z) && (n2 < n1) && (inShapeRange zs sh1 sh2)

        {-# NOINLINE listOfShape #-}
        listOfShape (sh :. n)
         = n : listOfShape sh

        {-# NOINLINE shapeOfList #-}
        shapeOfList xx
         = case xx of
                []      -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
                x:xs    -> shapeOfList xs :. x

        {-# INLINE deepSeq #-}
        deepSeq (sh :. n) x = deepSeq sh (n `seq` x)
