{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Hkl.Lattice ( Lattice(..)
                   , Cubic
                   , Tetragonal
                   , Orthorhombic
                   , Rhombohedral
                   , Hexagonal
                   , Monoclinic
                   , Triclinic
                   ) where

import           Numeric.Units.Dimensional.Prelude (Angle, Length)

--  Lattice

data Cubic
data Tetragonal
data Orthorhombic
data Rhombohedral
data Hexagonal
data Monoclinic
data Triclinic

data Lattice a where
   -- a = b = c, alpha = beta = gamma = 90
  Cubic ∷ Length Double
        → Lattice Cubic -- a = b = c, alpha = beta = gamma = 90
  -- a = b != c,  alpha = beta = gamma = 90
  Tetragonal ∷ Length Double -- a, b
             → Length Double -- c
             → Lattice Tetragonal
  -- a != b != c,  alpha = beta = gamma = 90
  Orthorhombic ∷ Length Double -- a
               → Length Double -- b
               → Length Double -- c
               → Lattice Orthorhombic
  -- a = b = c, alpha = beta = gamma != 90
  Rhombohedral ∷ Length Double -- a, b, c
               → Angle Double -- alpha, beta, gamma
               → Lattice Rhombohedral
  -- a = b != c, alpha = beta = 90, gamma = 120
  Hexagonal ∷ Length Double -- a, b
            → Length Double -- c
            → Lattice Hexagonal
  -- a != b != c, alpha = gamma = 90, beta != 90
  Monoclinic ∷ Length Double -- a
             → Length Double -- b
             → Length Double -- c
             → Angle Double -- beta
             → Lattice Monoclinic
  -- a != b != c, alpha != beta != gamma != 90
  Triclinic ∷ Length Double -- a
            → Length Double -- b
            → Length Double -- c
            → Angle Double -- alpha
            → Angle Double -- beta
            → Angle Double -- gamma
            → Lattice Triclinic

deriving instance Show (Lattice a)
