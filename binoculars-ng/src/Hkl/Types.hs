{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Hkl.Types ( AbsDirPath
                 , Beamline(..)
                 , ConfigContent(..)
                 , Key
                 , SampleName
                 , Section
                 , Timestamp(..)
                 , Timescan0(..)
                 , Trajectory
                 , beamlineUpper
                 , module X
                 ) where

import           Data.Aeson  (FromJSON (..), ToJSON (..))
import           Data.Char   (toUpper)
import           Data.String (IsString)
import           Data.Text   (Text)

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

-- ConfigContent

newtype ConfigContent
  = ConfigContent Text
  deriving (IsString, Show, FromJSON, ToJSON)

-- Key

-- newtype Key = Key Text
--   deriving (IsString, Show, FromJSON, ToJSON)

type Key = Text

-- Section

-- newtype Section = Section Text
--   deriving (IsString, Show, FromJSON, ToJSON)

type Section = Text

-- Timestamp

newtype Timestamp = Timestamp { unTimestamp :: Double }
  deriving Show

-- Timescan0

newtype Timescan0 = Timescan0 { unTimescan0 :: Double }
  deriving Show

--  Trajectory

type Trajectory = [[Double]]
