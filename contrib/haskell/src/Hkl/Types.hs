{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UnicodeSyntax #-}


module Hkl.Types ( AbsDirPath
                 , Beamline(..)
                 , beamlineUpper
                 , SampleName
                 , Timestamp(..)
                 , Trajectory
                   -- hdf5
                 , H5Path
                 , module X
                 ) where

import           Data.Char   (toUpper)

import           Hkl.H5
import           Hkl.Lattice as X

-- Common

type AbsDirPath = FilePath
type SampleName = String

--  Beamline

data Beamline = Diffabs | Sixs

instance Show Beamline where
  show Diffabs = "diffabs"
  show Sixs    = "sixs"

beamlineUpper ∷ Beamline → String
beamlineUpper b = [toUpper x | x ← show b]

-- Index

newtype Timestamp = Timestamp { unTimestamp :: Double }
  deriving Show

--  Trajectory

type Trajectory = [[Double]]
