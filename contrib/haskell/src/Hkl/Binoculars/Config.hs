{-# LANGUAGE GADTs             #-}
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
    , ProjectionType(..)
    , destination'
    , files
    , parseBinocularsConfig
    , overloadSampleWithConfig
    ) where


import           Control.Monad                     (filterM)
import           Control.Monad.Catch               (MonadThrow)
import           Control.Monad.Catch.Pure          (runCatch)
import           Data.Ini.Config                   (IniParser, fieldFlag,
                                                    fieldMb, fieldMbOf, fieldOf,
                                                    listWithSeparator, number,
                                                    section)
import           Data.List                         (isInfixOf)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, pack, replace,
                                                    takeWhile, unpack)
import           Data.Typeable                     (Typeable)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, DLength, Length,
                                                    Unit, degree, meter, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, File, Path,
                                                    fileExtension, parseAbsDir,
                                                    toFilePath)
import           Path.IO                           (listDir)
import           Text.Printf                       (printf)

import           Prelude                           hiding (length, takeWhile)

import           Hkl.Types

newtype ConfigRange a = ConfigRange [a]
  deriving (Eq, Show)

newtype DestinationTmpl =
  DestinationTmpl { unDestinationTmpl :: Text }
  deriving (Eq, Show)


data BinocularsDispatcher =
  BinocularsDispatcher { binocularsDispatcherNcore       :: Maybe Int
                       , binocularsDispatcherDestination :: DestinationTmpl
                       , binocularsDispatcherOverwrite   :: Bool
                       } deriving (Eq, Show)

data InputType = SixsFlyScanUhv
               | SixsFlyScanUhv2
  deriving (Eq, Show)

data BinocularsInput =
  BinocularsInput { binocularsInputItype                  :: InputType
                  , binocularsInputNexusdir               :: Path Abs Dir
                  , binocularsInputInputrange             :: Maybe (ConfigRange Int)
                  , binocularsInputCentralpixel           :: (Int, Int)
                  , binocularsInputSdd                    :: Length Double
                  , binocularsInputDetrot                 :: Maybe (Angle Double)
                  , binocularsInputAttenuationCoefficient :: Maybe Double
                  , binocularsInputMaskmatrix             :: Maybe Text
                  , binocularsInputA                      :: Maybe (Length Double)
                  , binocularsInputB                      :: Maybe (Length Double)
                  , binocularsInputC                      :: Maybe (Length Double)
                  , binocularsInputAlpha                  :: Maybe (Angle Double)
                  , binocularsInputBeta                   :: Maybe (Angle Double)
                  , binocularsInputGamma                  :: Maybe (Angle Double)
                  , binocularsInputUx                     :: Maybe (Angle Double)
                  , binocularsInputUy                     :: Maybe (Angle Double)
                  , binocularsInputUz                     :: Maybe (Angle Double)
                  } deriving (Eq, Show)

data ProjectionType = QxQyQzProjection
                    | HklProjection
  deriving (Eq, Show)

data BinocularsProjection =
  BinocularsProjection { binocularsProjectionPtype      :: ProjectionType
                       , binocularsProjectionResolution :: [Double]
                       -- , limits     :: Maybe [Double]
                       } deriving (Eq, Show)

data BinocularsConfig =
  BinocularsConfig { binocularsConfigDispatcher :: BinocularsDispatcher
                   , binocularsConfigInput      :: BinocularsInput
                   , binocularsConfigProjection :: BinocularsProjection
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

files :: BinocularsConfig -> IO [Path Abs File]
files c' = do
  (_, fs) <- listDir (binocularsInputNexusdir . binocularsConfigInput $ c')
  fs' <- filterM isHdf5 fs
  return $ case binocularsInputInputrange . binocularsConfigInput $ c' of
    Just r  -> filter (isInConfigRange r) fs'
    Nothing -> fs'
    where
      isHdf5 :: MonadThrow m => Path Abs File -> m Bool
      isHdf5 p = do
               let e = fileExtension p
               return  $ e `elem` [".h5", ".nxs"]

      matchIndex :: Path Abs File -> Int -> Bool
      matchIndex p n = printf "%05d" n `isInfixOf` toFilePath p

      isInConfigRange :: ConfigRange Int -> Path Abs File -> Bool
      isInConfigRange (ConfigRange []) _ = True
      isInConfigRange (ConfigRange [from]) p = any (matchIndex p) [from]
      isInConfigRange (ConfigRange [from, to]) p = any (matchIndex p) [from..to]
      isInConfigRange (ConfigRange (from:to:_)) p = any (matchIndex p) [from..to]


replace' :: Int -> Int -> DestinationTmpl -> FilePath
replace' f t = unpack . replace "{last}" (pack . show $ t) . replace "{first}" (pack . show $ f) . unDestinationTmpl

destination' :: ConfigRange Int -> DestinationTmpl -> FilePath
destination' (ConfigRange [])          = replace' 0 0
destination' (ConfigRange [from])      = replace' from from
destination' (ConfigRange [from, to])  = replace' from to
destination' (ConfigRange (from:to:_)) = replace' from to

overloadSampleWithConfig :: BinocularsConfig -> Sample Triclinic -> Sample Triclinic
overloadSampleWithConfig (BinocularsConfig _ i _) (Sample
                            name
                            (Triclinic a b c alpha beta gamma)
                            ux uy uz) =
    Sample name nlat nux nuy nuz
        where
          nlat = Triclinic
                 (fromMaybe a (binocularsInputA i))
                 (fromMaybe b (binocularsInputB i))
                 (fromMaybe c (binocularsInputC i))
                 (fromMaybe alpha (binocularsInputAlpha i))
                 (fromMaybe beta (binocularsInputBeta i))
                 (fromMaybe gamma (binocularsInputGamma i))

          go :: Parameter -> Maybe Double -> Parameter
          go (Parameter n v r) nv = Parameter n (fromMaybe v nv) r
          nux = go ux (fmap (/~ degree) (binocularsInputUx i))
          nuy = go uy (fmap (/~ degree) (binocularsInputUy i))
          nuz = go uz (fmap (/~ degree) (binocularsInputUz i))
