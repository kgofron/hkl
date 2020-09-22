{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
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
    , ConfigRange(..)
    , DestinationTmpl(..)
    , InputType(..)
    , ProjectionType(..)
    , destination'
    , files
    , getConfig
    , new
    , overloadSampleWithConfig
    , sampleConfig
    , update
    ) where


import           Control.Lens                      (makeLenses, (^.))
import           Control.Monad                     (filterM)
import           Control.Monad.Catch               (MonadThrow)
import           Control.Monad.Catch.Pure          (runCatch)
#if MIN_VERSION_extra(1, 6, 9)
#else
import           Data.Either.Extra                 (mapLeft, mapRight)
#endif
import           Data.Array.Repa.Index             (DIM2)
import           Data.Ini.Config.Bidir             (FieldValue (..), IniSpec,
                                                    bool, field, getIniValue,
                                                    ini, listWithSeparator,
                                                    number, parseIni, section,
                                                    serializeIni, text, (.=),
                                                    (.=?))
import           Data.List                         (isInfixOf)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, breakOn, drop, length,
                                                    pack, replace, strip,
                                                    takeWhile, unpack)
import           Data.Text.IO                      (putStr, readFile)
import           Data.Typeable                     (Typeable)
import           GHC.Exts                          (IsList)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, Quantity,
                                                    Unit, degree, meter, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, File, Path,
                                                    fileExtension, fromAbsDir,
                                                    parseAbsDir, toFilePath)
import           Path.IO                           (getCurrentDir, listDir)
import           Text.Printf                       (printf)

import           Prelude                           hiding (drop, length, putStr,
                                                    readFile, takeWhile)

import           Hkl.Detector
import           Hkl.Types
import           Paths_hkl


#if MIN_VERSION_extra(1, 6, 9)
--  The 'mapLeft' function takes a function and applies it to an Either value
-- iff the value takes the form @'Left' _@.
--
-- > mapLeft show (Left 1) == Left "1"
-- > mapLeft show (Right True) == Right True
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

--  The 'mapRight' function takes a function and applies it to an Either value
-- iff the value takes the form @'Right' _@.
--
-- > mapRight show (Left 1) == Left 1
-- > mapRight show (Right True) == Right "True"
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = fmap
#endif

newtype DestinationTmpl =
  DestinationTmpl { unDestinationTmpl :: Text }
  deriving (Eq, Show)

data InputType = SixsFlyScanUhv
               | SixsFlyScanUhv2
               | SixsSbsMedV
               | CristalK6C
  deriving (Eq, Show)

newtype ConfigRange a = ConfigRange [a]
  deriving (Eq, Show, IsList)

data ProjectionType = QxQyQzProjection
                    | HklProjection
  deriving (Eq, Show)

data BinocularsConfig = BinocularsConfig
  { _binocularsDispatcherNcore             :: Maybe Int
  , _binocularsDispatcherDestination       :: DestinationTmpl
  , _binocularsDispatcherOverwrite         :: Bool
  , _binocularsInputItype                  :: InputType
  , _binocularsInputNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsInputInputrange             :: Maybe (ConfigRange Int)
  , _binocularsInputDetector               :: Maybe (Detector PyFAI DIM2)
  , _binocularsInputCentralpixel           :: (Int, Int)
  , _binocularsInputSdd                    :: Length Double
  , _binocularsInputDetrot                 :: Maybe (Angle Double)
  , _binocularsInputAttenuationCoefficient :: Maybe Double
  , _binocularsInputMaskmatrix             :: Maybe Text
  , _binocularsInputA                      :: Maybe (Length Double)
  , _binocularsInputB                      :: Maybe (Length Double)
  , _binocularsInputC                      :: Maybe (Length Double)
  , _binocularsInputAlpha                  :: Maybe (Angle Double)
  , _binocularsInputBeta                   :: Maybe (Angle Double)
  , _binocularsInputGamma                  :: Maybe (Angle Double)
  , _binocularsInputUx                     :: Maybe (Angle Double)
  , _binocularsInputUy                     :: Maybe (Angle Double)
  , _binocularsInputUz                     :: Maybe (Angle Double)
  , _binocularsProjectionPtype             :: ProjectionType
  , _binocularsProjectionResolution        :: [Double]
  } deriving (Eq, Show)

makeLenses ''BinocularsConfig

binocularsConfigDefault :: BinocularsConfig
binocularsConfigDefault = BinocularsConfig
  { _binocularsDispatcherNcore = Nothing
  , _binocularsDispatcherDestination = DestinationTmpl "."
  , _binocularsDispatcherOverwrite = False
  , _binocularsInputItype = SixsFlyScanUhv
  , _binocularsInputNexusdir = Nothing
  , _binocularsInputInputrange = Nothing
  , _binocularsInputDetector = Just ImXpadS140
  , _binocularsInputCentralpixel = (0, 0)
  , _binocularsInputSdd = 1 *~ meter
  , _binocularsInputDetrot = Nothing
  , _binocularsInputAttenuationCoefficient = Nothing
  , _binocularsInputMaskmatrix = Nothing
  , _binocularsInputA = Nothing
  , _binocularsInputB = Nothing
  , _binocularsInputC = Nothing
  , _binocularsInputAlpha = Nothing
  , _binocularsInputBeta = Nothing
  , _binocularsInputGamma = Nothing
  , _binocularsInputUx = Nothing
  , _binocularsInputUy = Nothing
  , _binocularsInputUz = Nothing
  , _binocularsProjectionPtype = QxQyQzProjection
  , _binocularsProjectionResolution = [0.01, 0.01, 0.01]
  }

ms :: String
ms = "#;"

uncomment :: Text -> Text
uncomment = takeWhile (`notElem` ms)

number' :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number' = number { fvParse = fvParse number . uncomment}

destinationTmpl :: FieldValue DestinationTmpl
destinationTmpl = FieldValue { fvParse = parse, fvEmit = emit }
    where
      parse :: Text -> Either String DestinationTmpl
      parse = Right . DestinationTmpl . uncomment

      emit :: DestinationTmpl -> Text
      emit (DestinationTmpl t) = t

binocularsConfigSpec :: IniSpec BinocularsConfig ()
binocularsConfigSpec = do
  section "dispatcher" $ do
    binocularsDispatcherNcore .=? field "ncores" number'
    binocularsDispatcherDestination .= field "destination" destinationTmpl
    binocularsDispatcherOverwrite .= field "overwrite" bool
  section "input" $ do
    binocularsInputItype .= field "type" inputType
    binocularsInputNexusdir .=? field "nexusdir" pathAbsDir
    binocularsInputInputrange .=? field "inputrange" configRange
    binocularsInputDetector .=? field "detector" detector
    binocularsInputCentralpixel .= field "centralpixel" centralPixel
    binocularsInputSdd .= field "sdd" (numberUnit meter)
    binocularsInputDetrot .=? field "detrot" (numberUnit degree)
    binocularsInputAttenuationCoefficient .=? field "attenuationCoefficient" number'
    binocularsInputMaskmatrix .=? field "maskmatrix" text
    binocularsInputA .=? field "a" (numberUnit angstrom)
    binocularsInputB .=? field "b" (numberUnit angstrom)
    binocularsInputC .=? field "c" (numberUnit angstrom)
    binocularsInputAlpha  .=?field "alpha" (numberUnit degree)
    binocularsInputBeta  .=? field "beta" (numberUnit degree)
    binocularsInputGamma .=? field "gamma" (numberUnit degree)
    binocularsInputUx .=? field "ux" (numberUnit degree)
    binocularsInputUy .=? field "uy" (numberUnit degree)
    binocularsInputUz .=? field "uz" (numberUnit degree)
  section "projection" $ do
    binocularsProjectionPtype .= field "type" projectionType
    binocularsProjectionResolution .= field "resolution" (listWithSeparator "," number')

inputType :: FieldValue InputType
inputType = FieldValue { fvParse = parse . strip. uncomment, fvEmit = emit }
    where
      parse :: Text -> Either String InputType
      parse t
          | t == "sixs:flyscanuhv" = Right SixsFlyScanUhv
          | t == "sixs:flyscanuhv2" = Right SixsFlyScanUhv2
          | t == "sixs:sbsmedv" = Right SixsSbsMedV
          | t == "cristal:k6c" = Right CristalK6C
          | otherwise = Left ("Unsupported \"" ++ unpack t ++ "\" input format")

      emit :: InputType -> Text
      emit SixsFlyScanUhv  = "sixs:flyscanuhv"
      emit SixsFlyScanUhv2 = "sixs:flyscanuhv2"
      emit SixsSbsMedV     = "sixs:sbsmedv"
      emit CristalK6C      = "cristal:k6c"

projectionType :: FieldValue ProjectionType
projectionType = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
  where
    parse ::  Text -> Either String ProjectionType
    parse t
      | t == "hklprojection" = Right HklProjection
      | t == "qxqyqzprojection" = Right QxQyQzProjection
      | t == "sixs:qxqyqzprojection" = Right QxQyQzProjection
      | t == "sixs:hklprojection" = Right HklProjection
      | otherwise = Left ("Unsupported " ++ unpack t ++ " projection type")

    emit :: ProjectionType -> Text
    emit QxQyQzProjection = "qxqyqzprojection"
    emit HklProjection    = "hklprojection"

pathAbsDir :: FieldValue (Path Abs Dir)
pathAbsDir = FieldValue
  { fvParse = \t -> mapLeft show (runCatch . parseAbsDir . unpack $ t)
  , fvEmit = pack . fromAbsDir
  }

configRange :: (Num a, Read a, Show a, Typeable a) => FieldValue (ConfigRange a)
configRange = listWithSeparator "," number'

detector :: FieldValue (Detector PyFAI DIM2)
detector = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
    where
      parse :: Text -> Either String (Detector PyFAI DIM2)
      parse t
          | t == pack (show Xpad32) = Right Xpad32
          | t == pack (show ImXpadS140) = Right ImXpadS140
          | otherwise = Left  ("Unsupported " ++ unpack t ++ " detector type")

      emit ::  Detector PyFAI DIM2 -> Text
      emit = pack . show

centralPixel :: FieldValue (Int, Int)
centralPixel = pairWithSeparator' number' "," number'

--  Represents a field whose value is a pair of two other values
-- separated by a given string, whose individual values are described
-- by two different 'FieldValue' values.
pairWithSeparator' :: FieldValue l -> Text -> FieldValue r -> FieldValue (l, r)
pairWithSeparator' left sep right = FieldValue
  { fvParse = \ t ->
      let (leftChunk, rightChunk) = breakOn sep t
      in do
        x <- fvParse left leftChunk
        y <- fvParse right (drop (length sep) rightChunk)
        return (x, y)
  , fvEmit = \ (x, y) -> fvEmit left x <> sep <> fvEmit right y
  }

numberUnit :: (Num a, Fractional a, Read a, Show a, Typeable a) =>
             Unit m d a -> FieldValue (Quantity d a)
numberUnit u = FieldValue
  { fvParse = mapRight (*~ u) . fvParse number'
  , fvEmit = pack . show . (/~ u)
  }


files :: BinocularsConfig -> IO [Path Abs File]
files c = do
  (_, fs) <- listDir =<< case c ^. binocularsInputNexusdir of
                          Nothing  -> getCurrentDir
                          (Just d) -> pure d
  fs' <- filterM isHdf5 fs
  return $ case c ^. binocularsInputInputrange of
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

sampleConfig ::  BinocularsConfig -> Maybe (Sample Triclinic)
sampleConfig cf = do
  a <- _binocularsInputA cf
  b <- _binocularsInputB cf
  c <- _binocularsInputC cf
  alpha <- _binocularsInputAlpha cf
  beta <- _binocularsInputBeta cf
  gamma <- _binocularsInputGamma cf
  ux <- _binocularsInputUx cf
  uy <- _binocularsInputUy cf
  uz <- _binocularsInputUz cf
  let pux = Parameter "ux" (ux /~ degree) (Range 0  360)
  let puy = Parameter "uy" (uy /~ degree) (Range 0  360)
  let puz = Parameter "uz" (uz /~ degree) (Range 0  360)
  return $ Sample "triclinic" (Triclinic a b c alpha beta gamma) pux puy puz

overloadSampleWithConfig :: BinocularsConfig -> Sample Triclinic -> Sample Triclinic
overloadSampleWithConfig conf (Sample
                               name
                               (Triclinic a b c alpha beta gamma)
                               ux uy uz) =
    Sample name nlat nux nuy nuz
        where
          nlat = Triclinic
                 (fromMaybe a (_binocularsInputA conf))
                 (fromMaybe b (_binocularsInputB conf))
                 (fromMaybe c (_binocularsInputC conf))
                 (fromMaybe alpha (_binocularsInputAlpha conf))
                 (fromMaybe beta (_binocularsInputBeta conf))
                 (fromMaybe gamma (_binocularsInputGamma conf))

          go :: Parameter -> Maybe Double -> Parameter
          go (Parameter n v r) nv = Parameter n (fromMaybe v nv) r

          nux = go ux (fmap (/~ degree) (_binocularsInputUx conf))
          nuy = go uy (fmap (/~ degree) (_binocularsInputUy conf))
          nuz = go uz (fmap (/~ degree) (_binocularsInputUz conf))

getConfig :: Maybe FilePath -> IO (Either String BinocularsConfig)
getConfig mf = do
  cfg <- readFile =<< case mf of
                       Nothing -> getDataFileName "data/test/config_manip1.cfg"
                       (Just f) -> pure f
  let r = parseIni cfg (ini binocularsConfigDefault binocularsConfigSpec)
  return $ mapRight getIniValue r

new :: Maybe FilePath -> IO ()
new mf = do
  cwd <- case mf of
          (Just f) -> parseAbsDir f
          Nothing  -> getCurrentDir
  let conf = binocularsConfigDefault {_binocularsInputNexusdir = Just cwd}
  putStr $ serializeIni (ini conf binocularsConfigSpec)

update :: FilePath -> IO ()
update f = do
  cfg <- readFile f
  let eini = parseIni cfg (ini binocularsConfigDefault binocularsConfigSpec)
  putStr $ case eini of
             Left s  -> pack s
             Right v -> serializeIni (ini (getIniValue v) binocularsConfigSpec)
