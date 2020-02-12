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
    , ConfigRange(..)
    , DestinationTmpl(..)
    , InputType(..)
    , parseBinocularsConfig
    ) where


import           Control.Monad.Catch.Pure          (runCatch)
import           Data.Ini.Config                   (IniParser, fieldFlag,
                                                    fieldMb, fieldMbOf, fieldOf,
                                                    listWithSeparator, number,
                                                    section)
import           Data.Text                         (Text, takeWhile, unpack)
import           Data.Typeable                     (Typeable)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, DLength, Length,
                                                    Unit, degree, meter, (*~))
import           Path                              (Abs, Dir, Path, parseAbsDir)

import           Prelude                           hiding (length, takeWhile)

newtype ConfigRange a = ConfigRange [a]
  deriving (Eq, Show)

newtype DestinationTmpl =
  DestinationTmpl { unDestinationTmpl :: Text }
  deriving (Eq, Show)


data BinocularsDispatcher =
  BinocularsDispatcher { ncore       :: Maybe Int
                       , destination :: DestinationTmpl
                       , overwrite   :: Bool
                       } deriving (Eq, Show)

data InputType = SixsFlyScanUhv
               | SixsFlyScanUhv2
  deriving (Eq, Show)

data BinocularsInput =
  BinocularsInput { itype                  :: InputType
                  , nexusdir               :: Path Abs Dir
                  , inputrange             :: Maybe (ConfigRange Int)
                  , centralpixel           :: (Int, Int)
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

data ProjectionType = QxQyQzProjection
                    | HklProjection
  deriving (Eq, Show)

data BinocularsProjection =
  BinocularsProjection { ptype      :: ProjectionType
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

parseInputType :: Text -> Either String InputType
parseInputType t
  | t == "sixs:flyscanuhv" = Right SixsFlyScanUhv
  | t == "sixs:flyscanuhv2" = Right SixsFlyScanUhv2
  | otherwise = Left ("Unsupported " ++ unpack t ++ " input format")

parseProjectionType :: Text -> Either String ProjectionType
parseProjectionType t
  | t == "sixs:qxqyqzprojection" = Right QxQyQzProjection
  | t == "sixs:hklprojection" = Right HklProjection
  | otherwise = Left ("Unsupported " ++ unpack t ++ " projection type")

pathAbsDir :: Text -> Either String (Path Abs Dir)
pathAbsDir t = do
  let d = runCatch $ parseAbsDir (unpack t)
  case d of
    Right v -> Right v
    Left e  -> Left $ show e

parseRange :: (Num a, Read a, Typeable a) => Text -> Either String (ConfigRange a)
parseRange t = case listWithSeparator' "," number' t of
  Right v -> Right (ConfigRange v)
  Left e  -> Left e

parseDestinationTmpl :: Text -> Either String DestinationTmpl
parseDestinationTmpl = Right . DestinationTmpl . uncomment

parseCentralPixel :: Text -> Either String (Int, Int)
parseCentralPixel t = case listWithSeparator' "," number' t of
  Right v -> go v
  Left e  -> Left e
  where
      go :: [Int] -> Either String (Int, Int)
      go []      = Left "Please provide central pixel coordinates `x`, `y`"
      go [_]     = Left "Please provide central pixel coordinates `y`"
      go [x, y]  = Right (x, y)
      go (x:y:_) = Right (x, y)

length :: (Num a, Fractional a, Read a, Typeable a) => Unit m DLength a -> Text -> Either String (Length a)
length u t = case number' t of
  (Right v) -> Right (v *~ u)
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
                             <*> fieldOf "destination" parseDestinationTmpl
                             <*> fieldFlag "overwrite"
                           )
  <*> section "input" (BinocularsInput
                        <$> fieldOf "type" parseInputType
                        <*> fieldOf "nexusdir" pathAbsDir
                        <*> fieldMbOf "inputrange" parseRange
                        <*> fieldOf "centralpixel" parseCentralPixel
                        <*> fieldOf "sdd" (length meter)
                        <*> fieldMbOf "detrot" angle
                        <*> fieldMbOf "attenuationCoefficient" number'
                        <*> fieldMb "maskmatrix"
                        <*> fieldMbOf "a" (length angstrom)
                        <*> fieldMbOf "b" (length angstrom)
                        <*> fieldMbOf "c" (length angstrom)
                        <*> fieldMbOf "alpha" angle
                        <*> fieldMbOf "beta" angle
                        <*> fieldMbOf "gamma" angle
                        <*> fieldMbOf "ux" angle
                        <*> fieldMbOf "uy" angle
                        <*> fieldMbOf "uz" angle
                      )
  <*> section "projection" (BinocularsProjection
                             <$> fieldOf "type" parseProjectionType
                             <*> fieldOf "resolution" (listWithSeparator' "," number')
                             -- <*> fieldMbOf "limits" (listWithSeparator' "," number')
                           )
