{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}

module Hkl.C.Lattice
       ( HklLattice
       , newLattice
       , withLattice
       ) where

import           Prelude                           hiding (max, min)

import           Foreign                           (ForeignPtr, FunPtr, Ptr,
                                                    newForeignPtr, nullPtr,
                                                    withForeignPtr)
import           Foreign.C                         (CDouble (..))

import           Hkl.Lattice
import           Numeric.Units.Dimensional.Prelude (degree, meter, nano, radian,
                                                    (*~), (/~))

#include "hkl.h"

#if __GLASGOW_HASKELL__ <= 710
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- private types

data HklLattice

-- Lattice

withLattice :: Lattice a -> (Ptr HklLattice -> IO r) -> IO r
withLattice l func = do
  fptr <- newLattice l
  withForeignPtr fptr func

newLattice' :: CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> IO (ForeignPtr HklLattice)
newLattice' a b c alpha beta gamma = do
  lattice <- c_hkl_lattice_new a b c alpha beta gamma nullPtr
  newForeignPtr c_hkl_lattice_free lattice

newLattice :: Lattice a -> IO (ForeignPtr HklLattice)
newLattice  (Cubic la) = do
  let a = CDouble (la /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Tetragonal la lc) = do
  let a = CDouble (la /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a c alpha alpha alpha
newLattice  (Orthorhombic la lb lc) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a b c alpha alpha alpha
newLattice (Rhombohedral la aalpha) = do
  let a = CDouble (la /~ nano meter)
  let alpha = CDouble (aalpha /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Hexagonal la lc) = do
  let a = CDouble (la /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let gamma = CDouble ((120 *~ degree) /~ radian)
  newLattice' a a c alpha alpha gamma
newLattice (Monoclinic la lb lc abeta) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let beta = CDouble (abeta /~ radian)
  newLattice' a b c alpha beta alpha
newLattice (Triclinic la lb lc aalpha abeta agamma) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble (aalpha /~ radian)
  let beta = CDouble (abeta /~ radian)
  let gamma = CDouble (agamma /~ radian)
  newLattice' a b c alpha beta gamma

foreign import ccall unsafe "hkl.h hkl_lattice_new"
  c_hkl_lattice_new :: CDouble -- a
                    -> CDouble -- b
                    -> CDouble -- c
                    -> CDouble -- alpha
                    -> CDouble -- beta
                    -> CDouble -- gamma
                    -> Ptr () -- gerror
                    -> IO (Ptr HklLattice)

foreign import ccall unsafe "hkl.h &hkl_lattice_free"
  c_hkl_lattice_free :: FunPtr (Ptr HklLattice -> IO ())
