{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Hkl.Lattice
  ( Degree(..)
  , Lattice(..)
  , NanoMeter(..)
  , withLattice
  ) where

import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Foreign                           (ForeignPtr, Ptr,
                                                    newForeignPtr, nullPtr,
                                                    withForeignPtr)
import           Foreign.C.Types                   (CDouble (..))
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, nano, radian, (*~),
                                                    (/~))
import           Test.QuickCheck                   (Arbitrary (..))

import           Hkl.C.Hkl

newtype NanoMeter = NanoMeter { unNanoMeter :: Length Double }
    deriving (Eq, Show)

instance Arbitrary NanoMeter where
  arbitrary = NanoMeter . (*~ nano meter) <$> arbitrary

instance FromJSON NanoMeter where
  parseJSON = fmap (NanoMeter . (*~ nano meter)) . parseJSON

instance ToJSON NanoMeter where
  toJSON = toJSON . (/~ nano meter) . unNanoMeter

newtype Degree = Degree { unDegree :: Angle Double }
    deriving (Eq, Show)

instance Arbitrary Degree where
  arbitrary = Degree . (*~ degree) <$> arbitrary

instance FromJSON Degree where
  parseJSON = fmap (Degree . (*~ degree)) . parseJSON

instance ToJSON Degree where
  toJSON = toJSON . (/~ degree) . unDegree


--  Lattice

data Lattice = Cubic -- a = b = c, alpha = beta = gamma = 90
               NanoMeter
             | Tetragonal -- a = b != c,  alpha = beta = gamma = 90
               NanoMeter -- a, b
               NanoMeter -- c
             | Orthorhombic -- a != b != c,  alpha = beta = gamma = 90
               NanoMeter -- a
               NanoMeter -- b
               NanoMeter -- c
             | Rhombohedral -- a = b = c, alpha = beta = gamma != 90
               NanoMeter -- a, b, c
               Degree -- alpha, beta, gamma
             | Hexagonal -- a = b != c, alpha = beta = 90, gamma = 120
               NanoMeter -- a, b
               NanoMeter -- c
             | Monoclinic -- a != b != c, alpha = gamma = 90, beta != 90
               NanoMeter -- a
               NanoMeter -- b
               NanoMeter -- c
               Degree -- beta
             | Triclinic -- a != b != c, alpha != beta != gamma != 90
               Double -- a
               Double -- b
               Double -- c
               Degree -- alpha
               Degree -- beta
               Degree -- gamma
             deriving (Eq, Generic, FromJSON, Show, ToJSON)

withLattice :: Lattice -> (Ptr C'HklLattice -> IO r) -> IO r
withLattice l func = do
  fptr <- newLattice l
  withForeignPtr fptr func

newLattice' :: CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> IO (ForeignPtr C'HklLattice)
newLattice' a b c alpha beta gamma = do
  lattice <- c'hkl_lattice_new a b c alpha beta gamma nullPtr
  newForeignPtr p'hkl_lattice_free lattice

newLattice :: Lattice -> IO (ForeignPtr C'HklLattice)
newLattice  (Cubic (NanoMeter la)) = do
  let a = CDouble (la /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Tetragonal (NanoMeter la) (NanoMeter lc)) = do
  let a = CDouble (la /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a c alpha alpha alpha
newLattice  (Orthorhombic (NanoMeter la) (NanoMeter lb) (NanoMeter lc)) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a b c alpha alpha alpha
newLattice (Rhombohedral (NanoMeter la) (Degree aalpha)) = do
  let a = CDouble (la /~ nano meter)
  let alpha = CDouble (aalpha /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Hexagonal (NanoMeter la) (NanoMeter lc)) = do
  let a = CDouble (la /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let gamma = CDouble ((120 *~ degree) /~ radian)
  newLattice' a a c alpha alpha gamma
newLattice (Monoclinic (NanoMeter la) (NanoMeter lb) (NanoMeter lc) (Degree abeta)) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let beta = CDouble (abeta /~ radian)
  newLattice' a b c alpha beta alpha
newLattice (Triclinic la lb lc (Degree aalpha) (Degree abeta) (Degree agamma)) = do
  let a = CDouble la
  let b = CDouble lb
  let c = CDouble lc
  let alpha = CDouble (aalpha /~ radian)
  let beta = CDouble (abeta /~ radian)
  let gamma = CDouble (agamma /~ radian)
  newLattice' a b c alpha beta gamma
