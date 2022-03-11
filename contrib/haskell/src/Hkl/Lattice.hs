{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Hkl.Lattice
  ( Degree(..)
  , Lattice(..)
  , NanoMeter(..)
  ) where

import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, nano, (*~), (/~))

newtype NanoMeter = NanoMeter { unNanoMeter :: Length Double }
    deriving (Eq, Show)

instance FromJSON NanoMeter where
  parseJSON = fmap (NanoMeter . (*~ nano meter)) . parseJSON

instance ToJSON NanoMeter where
  toJSON = toJSON . (/~ nano meter) . unNanoMeter

newtype Degree = Degree { unDegree :: Angle Double }
    deriving (Eq, Show)

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
               NanoMeter -- a
               NanoMeter -- b
               NanoMeter -- c
               Degree -- alpha
               Degree -- beta
               Degree -- gamma
             deriving (Eq, Generic, FromJSON, Show, ToJSON)
