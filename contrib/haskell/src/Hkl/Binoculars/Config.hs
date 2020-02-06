{-# LANGUAGE OverloadedStrings #-}
{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config
    ( BinocularsConfig(..)
    , BinocularsDispatcher(..)
    , BinocularsInput(..)
    , BinocularsProjection(..)
    , parseBinocularsConfig
    ) where

import           Data.Ini.Config                   (IniParser, field, fieldFlag,
                                                    fieldMb, fieldMbOf, fieldOf,
                                                    listWithSeparator, number,
                                                    section)
import           Data.Text                         (Text, takeWhile)
import           Data.Typeable                     (Typeable)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree, (*~))

import           Prelude                           hiding (length, takeWhile)

data BinocularsDispatcher =
  BinocularsDispatcher { ncore       :: Maybe Int
                       , destination :: Text
                       , overwrite   :: Bool
                       } deriving (Eq, Show)

data BinocularsInput =
  BinocularsInput { itype                  :: Text
                  , nexusdir               :: Text
                  , centralpixel           :: [Int]
                  , sdd                    :: Length Double
                  , detrot                 :: Maybe (Angle Double)
                  , attenuationCoefficient :: Maybe Double
                  , maskmatrix             :: Maybe Text
                  , a                      :: Maybe (Length Double)
                  , b                      :: Maybe (Length Double)
                  , c                      :: Maybe (Length Double)
                  , alpha                  :: Maybe (Angle Double)
                  , beta                   :: Maybe (Angle Double)
                  , gamma                  :: Maybe (Angle Double)
                  , ux                     :: Maybe (Angle Double)
                  , uy                     :: Maybe (Angle Double)
                  , uz                     :: Maybe (Angle Double)
                  } deriving (Eq, Show)

data BinocularsProjection =
  BinocularsProjection { ptype      :: Text
                       , resolution :: [Double]
                       -- , limits     :: Maybe [Double]
                       } deriving (Eq, Show)

data BinocularsConfig =
  BinocularsConfig { dispatcher :: BinocularsDispatcher
                   , input      :: BinocularsInput
                   , projection :: BinocularsProjection
                   } deriving (Eq, Show)

ms :: String
ms = "#;"

uncomment :: Text -> Text
uncomment = takeWhile (`notElem` ms)

number' :: (Num a, Read a, Typeable a) => Text -> Either String a
number' = number . uncomment

length :: (Num a, Read a, Fractional a, Typeable a) => Text -> Either String (Length a)
length t = case number' t of
  (Right v) -> Right (v *~ angstrom)
  (Left e)  -> Left e

angle :: (Num a, Read a, Floating a, Typeable a) => Text -> Either String (Angle a)
angle t = case number' t of
  (Right v) -> Right (v *~ degree)
  (Left e)  -> Left e

listWithSeparator' :: Text -> (Text -> Either String a) -> Text -> Either String [a]
listWithSeparator' s p = listWithSeparator s p . uncomment

parseBinocularsConfig :: IniParser BinocularsConfig
parseBinocularsConfig = BinocularsConfig
  <$> section "dispatcher" (BinocularsDispatcher
                             <$> fieldMbOf "ncores" number'
                             <*> field "destination"
                             <*> fieldFlag "overwrite"
                           )
  <*> section "input" (BinocularsInput
                        <$> field "type"
                        <*> field "nexusdir"
                        <*> fieldOf "centralpixel" (listWithSeparator' "," number')
                        <*> fieldOf "sdd" length
                        <*> fieldMbOf "detrot" angle
                        <*> fieldMbOf "attenuationCoefficient" number'
                        <*> fieldMb "maskmatrix"
                        <*> fieldMbOf "a" length
                        <*> fieldMbOf "b" length
                        <*> fieldMbOf "c" length
                        <*> fieldMbOf "alpha" angle
                        <*> fieldMbOf "beta" angle
                        <*> fieldMbOf "gamma" angle
                        <*> fieldMbOf "ux" angle
                        <*> fieldMbOf "uy" angle
                        <*> fieldMbOf "uz" angle
                      )
  <*> section "projection" (BinocularsProjection
                             <$> field "type"
                             <*> fieldOf "resolution" (listWithSeparator' "," number')
                             -- <*> fieldMbOf "limits" (listWithSeparator' "," number')
                           )
