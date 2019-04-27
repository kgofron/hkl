{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Types ( AbsDirPath
                 , Beamline(..)
                 , beamlineUpper
                 , Mode(..)
                 , Engine(..)
                 , SampleName
                 , Sample(..)
                 , Source(..)
                 , Trajectory
                 , WaveLength
                   -- hdf5
                 , H5Path
                 , module X
                 ) where

import Data.Char (toUpper)

import Hkl.Types.Parameter as X
import Hkl.H5
import Hkl.Lattice
import Numeric.Units.Dimensional.Prelude (Length)

-- Common

type AbsDirPath = FilePath
type SampleName = String

-- | Beamline

data Beamline = Diffabs | Sixs

instance Show Beamline where
  show Diffabs = "diffabs"
  show Sixs = "sixs"

beamlineUpper ∷ Beamline → String
beamlineUpper b = [toUpper x | x ← show b]

-- | Engine

data Mode
  = Mode
    String -- ^ name
    [Parameter] -- ^ parameters of the @Mode@
  deriving (Show)

data Engine
  = Engine
    String -- ^ name
    [Parameter] -- ^ pseudo axes values of the @Engine@
    Mode -- ^ current Mode
  deriving (Show)

-- | Sample

data Sample a
  = Sample
    String -- ^ name of the sample
    (Lattice a) -- ^ the lattice of the sample
    Parameter -- ^ ux
    Parameter -- ^ uy
    Parameter -- ^ uz
  deriving (Show)

-- | Source

type WaveLength = Length Double

newtype Source = Source WaveLength deriving (Show)

-- | Trajectory

type Trajectory = [[Double]]
