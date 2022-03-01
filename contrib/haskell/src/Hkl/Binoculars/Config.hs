{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config
    ( BinocularsConfig(..)
    , BinocularsPreConfig(..)
    , ConfigRange(..)
    , DestinationTmpl(..)
    , InputRange(..)
    , InputTmpl(..)
    , InputType(..)
    , Limits(..)
    , ProjectionType(..)
    , SurfaceOrientation(..)
    , auto
    , centralPixel
    , configRangeP
    , combineWithCmdLineArgs
    , destination'
    , files
    , getConfig
    , getMask
    , getResolution
    , inputTmpl
    , inputType
    , limitsP
    , new
    , numberUnit
    , overloadSampleWithConfig
    , parsable
    , pathAbsDir
    , sampleConfig
    , surfaceOrientation
    , update
    ) where


import           Control.Applicative               (many, (<|>))
import           Control.Lens                      (makeLenses, (^.))
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.Catch.Pure          (runCatch)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Logger              (MonadLogger)
import           Data.Array.Repa.Index             (DIM2)
import           Data.Attoparsec.Text              (Parser, char, decimal,
                                                    double, parseOnly, satisfy,
                                                    sepBy, sepBy1')
import           Data.Either.Extra                 (mapLeft, mapRight)
import           Data.Ini.Config.Bidir             (FieldValue (..), IniSpec,
                                                    bool, field, getIniValue,
                                                    ini, listWithSeparator,
                                                    number, parseIni, section,
                                                    serializeIni, text, (.=),
                                                    (.=?))
import           Data.List                         (isInfixOf, length)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, breakOn, drop, empty,
                                                    findIndex, intercalate,
                                                    length, lines, pack,
                                                    replace, singleton, strip,
                                                    take, takeWhile, toLower,
                                                    unlines, unpack, unwords)
import           Data.Text.IO                      (putStr, readFile)
import           Data.Typeable                     (Typeable)
import           GHC.Exts                          (IsList)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, Quantity,
                                                    Unit, degree, meter, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, File, Path, Rel,
                                                    fileExtension, filename,
                                                    fromAbsDir, parseAbsDir,
                                                    toFilePath)
import           Path.IO                           (getCurrentDir, listDir)
import           Text.Printf                       (printf)

import           Prelude                           hiding (drop, length, lines,
                                                    putStr, readFile, take,
                                                    takeWhile, unlines, unwords)

import           Hkl.Detector
import           Hkl.Types
import           Paths_hkl


data HklBinocularsConfigException = NoFilesInTheGivenDirectory (Path Abs Dir)
                                  | NoDataFilesInTheGivenDirectory (Path Abs Dir)
                                  | NoFilesInRangeInTheGivenDirectory (Path Abs Dir) ConfigRange
                                  | ResolutionNotCompatibleWithProjectionNbOfCoordinates [Double] Int
    deriving (Show)
instance Exception HklBinocularsConfigException

newtype DestinationTmpl =
  DestinationTmpl { unDestinationTmpl :: Text }
  deriving (Eq, Show)

newtype InputTmpl = InputTmpl { unInputTmpl :: Text }
  deriving (Eq, Show)

data InputType = CristalK6C
               | MarsFlyscan
               | MarsSbs
               | SixsFlyMedH
               | SixsFlyMedV
               | SixsFlyMedVEiger
               | SixsFlyMedVS70
               | SixsFlyScanUhv
               | SixsFlyScanUhv2
               | SixsFlyScanUhvUfxc
               | SixsSbsFixedDetector
               | SixsSbsMedH
               | SixsSbsMedV
               | SixsSbsMedVFixDetector
  deriving (Eq, Show)

data InputRange = InputRangeSingle Int
                | InputRangeFromTo Int Int
                deriving (Eq, Show)

newtype ConfigRange = ConfigRange [InputRange]
  deriving (Eq, Show, IsList)

data SurfaceOrientation = SurfaceOrientationVertical
                        | SurfaceOrientationHorizontal
  deriving (Eq, Show)

instance Enum SurfaceOrientation where
  fromEnum SurfaceOrientationVertical   = 0
  fromEnum SurfaceOrientationHorizontal = 1

  toEnum 0         = SurfaceOrientationVertical
  toEnum 1         = SurfaceOrientationHorizontal
  toEnum unmatched = error ("Key.toEnum: Cannot match " ++ show unmatched)

data ProjectionType = QparQperProjection
                    | QxQyQzProjection
                    | HklProjection
  deriving (Eq, Show)

data Limits = Limits (Maybe Double) (Maybe Double)
  deriving (Eq, Show)

class HasFieldValue a where
  fieldvalue :: FieldValue a

class FieldParsable a where
  fieldParser :: Parser a
  fieldEmitter :: a -> Text

ms :: String
ms = "#;"

uncomment :: Text -> Text
uncomment = takeWhile (`notElem` ms)

parsable :: FieldParsable a => FieldValue a
parsable = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
  where
    parse ::  FieldParsable a => Text -> Either String a
    parse = parseOnly fieldParser

    emit ::  FieldParsable a => a -> Text
    emit = fieldEmitter

auto :: HasFieldValue a => FieldValue a
auto = fieldvalue

-------------------------
-- BinocularsPreConfig --
-------------------------

data BinocularsPreConfig = BinocularsPreConfig
  { _binocularsPreConfigProjectionType :: ProjectionType }
  deriving (Eq, Show)

makeLenses ''BinocularsPreConfig

binocularsPreConfigDefault :: BinocularsPreConfig
binocularsPreConfigDefault = BinocularsPreConfig
  { _binocularsPreConfigProjectionType = QxQyQzProjection }

binocularsPreConfigSpec :: IniSpec BinocularsPreConfig ()
binocularsPreConfigSpec = do
  section "projection" $ do
    binocularsPreConfigProjectionType .= field "type" parsable

----------------------
-- BinocularsConfig --
----------------------

data BinocularsConfig = BinocularsConfig
  { _binocularsDispatcherNcore             :: Maybe Int
  , _binocularsDispatcherDestination       :: DestinationTmpl
  , _binocularsDispatcherOverwrite         :: Bool
  , _binocularsInputItype                  :: InputType
  , _binocularsInputNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsInputTmpl                   :: Maybe InputTmpl
  , _binocularsInputInputRange             :: Maybe ConfigRange
  , _binocularsInputDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsInputCentralpixel           :: (Int, Int)
  , _binocularsInputSdd                    :: Length Double
  , _binocularsInputDetrot                 :: Maybe (Angle Double)
  , _binocularsInputAttenuationCoefficient :: Maybe Double
  , _binocularsInputSurfaceOrientation     :: Maybe SurfaceOrientation
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
  , _binocularsInputWavelength             :: Maybe (Length Double)
  , _binocularsProjectionPtype             :: ProjectionType
  , _binocularsProjectionResolution        :: [Double]
  , _binocularsProjectionLimits            :: Maybe [Limits]
  } deriving (Eq, Show)

makeLenses ''BinocularsConfig

binocularsConfigDefault :: BinocularsConfig
binocularsConfigDefault = BinocularsConfig
  { _binocularsDispatcherNcore = Nothing
  , _binocularsDispatcherDestination = DestinationTmpl "."
  , _binocularsDispatcherOverwrite = False
  , _binocularsInputItype = SixsFlyScanUhv
  , _binocularsInputNexusdir = Nothing
  , _binocularsInputTmpl = Nothing
  , _binocularsInputInputRange = Nothing
  , _binocularsInputDetector = Nothing
  , _binocularsInputCentralpixel = (0, 0)
  , _binocularsInputSdd = 1 *~ meter
  , _binocularsInputDetrot = Nothing
  , _binocularsInputAttenuationCoefficient = Nothing
  , _binocularsInputSurfaceOrientation = Just SurfaceOrientationVertical
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
  , _binocularsInputWavelength = Nothing
  , _binocularsProjectionPtype = QxQyQzProjection
  , _binocularsProjectionResolution = [0.01, 0.01, 0.01]
  , _binocularsProjectionLimits = Nothing
  }

number' :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number' = Data.Ini.Config.Bidir.number { fvParse = fvParse Data.Ini.Config.Bidir.number . uncomment}

instance HasFieldValue Bool where
  fieldvalue = bool

instance HasFieldValue Int where
  fieldvalue = number'

instance HasFieldValue Double where
  fieldvalue = number'

instance HasFieldValue DestinationTmpl where
  fieldvalue = FieldValue { fvParse = Right . DestinationTmpl . uncomment
                          , fvEmit = \(DestinationTmpl t) -> t
                          }

inputTmpl :: FieldValue InputTmpl
inputTmpl = FieldValue { fvParse = parse, fvEmit = emit }
    where
      parse :: Text -> Either String InputTmpl
      parse = Right . InputTmpl . uncomment

      emit :: InputTmpl -> Text
      emit (InputTmpl t) = t

binocularsConfigSpec :: IniSpec BinocularsConfig ()
binocularsConfigSpec = do
  section "dispatcher" $ do
    binocularsDispatcherNcore .=? field "ncores" auto
    binocularsDispatcherDestination .= field "destination" auto
    binocularsDispatcherOverwrite .= field "overwrite" auto
  section "input" $ do
    binocularsInputItype .= field "type" inputType
    binocularsInputNexusdir .=? field "nexusdir" pathAbsDir
    binocularsInputTmpl .=? field "inputtmpl" inputTmpl
    binocularsInputInputRange .=? field "inputrange" parsable
    binocularsInputDetector .=? field "detector" auto
    binocularsInputCentralpixel .= field "centralpixel" centralPixel
    binocularsInputSdd .= field "sdd" (numberUnit meter)
    binocularsInputDetrot .=? field "detrot" (numberUnit degree)
    binocularsInputAttenuationCoefficient .=? field "attenuation_coefficient" auto
    binocularsInputSurfaceOrientation .=? field "surface_orientation" surfaceOrientation
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
    binocularsInputWavelength .=? field "wavelength" (numberUnit angstrom)
  section "projection" $ do
    binocularsProjectionPtype .= field "type" parsable
    binocularsProjectionResolution .= field "resolution" (listWithSeparator "," auto)
    binocularsProjectionLimits .=? field "limits" parsable

inputType :: FieldValue InputType
inputType = FieldValue { fvParse = parse . strip. uncomment, fvEmit = emit }
    where
      parse :: Text -> Either String InputType
      parse t
          | toLower t == emit CristalK6C = Right CristalK6C
          | toLower t == emit MarsFlyscan = Right MarsFlyscan
          | toLower t == emit SixsFlyMedH = Right SixsFlyMedH
          | toLower t == emit SixsFlyMedV = Right SixsFlyMedV
          | toLower t == emit SixsFlyMedVEiger = Right SixsFlyMedVEiger
          | toLower t == emit SixsFlyMedVS70 = Right SixsFlyMedVS70
          | toLower t == emit SixsFlyScanUhv = Right SixsFlyScanUhv
          | toLower t == emit SixsFlyScanUhv2 = Right SixsFlyScanUhv2
          | toLower t == emit SixsFlyScanUhvUfxc = Right SixsFlyScanUhvUfxc
          | toLower t == emit SixsSbsFixedDetector = Right SixsSbsFixedDetector
          | toLower t == emit SixsSbsMedH = Right SixsSbsMedH
          | toLower t == emit SixsSbsMedV = Right SixsSbsMedV
          | toLower t == emit SixsSbsMedVFixDetector = Right SixsSbsMedVFixDetector
          | otherwise = Left ("Unsupported \"" ++ unpack t ++ "\" input format")

      emit :: InputType -> Text
      emit CristalK6C             = "cristal:k6c"
      emit MarsFlyscan            = "mars:flyscan"
      emit MarsSbs                = "mars:sbs"
      emit SixsFlyMedH            = "sixs:flymedh"
      emit SixsFlyMedV            = "sixs:flymedv"
      emit SixsFlyMedVEiger       = "sixs:flymedveiger"
      emit SixsFlyMedVS70         = "sixs:flymedvs70"
      emit SixsFlyScanUhv         = "sixs:flyscanuhv"
      emit SixsFlyScanUhv2        = "sixs:flyscanuhv2"
      emit SixsFlyScanUhvUfxc     = "sixs:flyscanuhvufxc"
      emit SixsSbsFixedDetector   = "sixs:sbsfixeddetector"
      emit SixsSbsMedH            = "sixs:sbsmedh"
      emit SixsSbsMedV            = "sixs:sbsmedv"
      emit SixsSbsMedVFixDetector = "sixs:sbsmedvfixdetector"

surfaceOrientation :: FieldValue SurfaceOrientation
surfaceOrientation = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
  where
    parse ::  Text -> Either String SurfaceOrientation
    parse t
      | t == emit SurfaceOrientationVertical = Right SurfaceOrientationVertical
      | t == emit SurfaceOrientationHorizontal = Right SurfaceOrientationHorizontal
      | otherwise = Left ("Unsupported " ++ unpack t ++ " surface orientation (vertical or horizontal)")

    emit :: SurfaceOrientation -> Text
    emit SurfaceOrientationVertical   = "vertical"
    emit SurfaceOrientationHorizontal = "horizontal"


instance FieldParsable ProjectionType where
  fieldParser = "hkl" *> return HklProjection
                <|> "qparqper" *> return QparQperProjection
                <|> "qxqyqz" *> return QxQyQzProjection
                <|> "sixs:qxqyqzprojection" *> return QxQyQzProjection
                <|> "sixs:hklprojection" *> return HklProjection

  fieldEmitter QparQperProjection = "qparqper"
  fieldEmitter QxQyQzProjection   = "qxqyqz"
  fieldEmitter HklProjection      = "hkl"

pathAbsDir :: FieldValue (Path Abs Dir)
pathAbsDir = FieldValue
  { fvParse = \t -> mapLeft show (runCatch . parseAbsDir . unpack $ t)
  , fvEmit = pack . fromAbsDir
  }


instance FieldParsable InputRange where
  fieldParser = inputRangeP

  fieldEmitter (InputRangeSingle f)   = pack $ printf "%d" f
  fieldEmitter (InputRangeFromTo f t) = pack $ printf "%d-%d" f t

inputRangeP :: Parser InputRange
inputRangeP = inputRangeFromToP <|> inputRangeP'
  where
    inputRangeFromToP :: Parser InputRange
    inputRangeFromToP =  InputRangeFromTo
                         <$> decimal <* char '-'
                         <*> decimal

    inputRangeP' :: Parser InputRange
    inputRangeP' = InputRangeSingle <$> decimal

instance FieldParsable ConfigRange where
  fieldParser = configRangeP

  fieldEmitter (ConfigRange is) = unwords $ map fieldEmitter is


configRangeP :: Parser ConfigRange
configRangeP = ConfigRange <$> (inputRangeP `sepBy` many (satisfy isSep))
    where
      isSep :: Char -> Bool
      isSep c = c == ' ' || c == ','

instance  HasFieldValue (Detector Hkl DIM2) where
  fieldvalue = FieldValue
               { fvParse = parseDetector2D . strip . uncomment
               , fvEmit = \(Detector2D _ name _) -> pack name
               }

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
        y <- fvParse right (drop (Data.Text.length sep) rightChunk)
        return (x, y)
  , fvEmit = \ (x, y) -> fvEmit left x <> sep <> fvEmit right y
  }

numberUnit :: (Num a, Fractional a, Read a, Show a, Typeable a) =>
             Unit m d a -> FieldValue (Quantity d a)
numberUnit u = FieldValue
  { fvParse = mapRight (*~ u) . fvParse number'
  , fvEmit = pack . show . (/~ u)
  }

limitsP' :: Parser Limits
limitsP' = Limits
           <$> lim <* char ':'
           <*> lim
  where
    lim :: Parser (Maybe Double)
    lim = (Just <$> double) <|> return Nothing

limitsP :: Parser [Limits]
limitsP = char '[' *> limitsP' `sepBy1'` char ',' <* char ']'


instance (FieldParsable [Limits]) where
  fieldParser = limitsP
  fieldEmitter ls = singleton '['  <> intercalate "," (map emit' ls) <> singleton ']'
    where
      emit' :: Limits -> Text
      emit' (Limits from to) = tolim from <> singleton ':' <> tolim to

      tolim (Just l) = pack $ printf "%f" l
      tolim Nothing  = empty

files :: (MonadThrow m, MonadIO m) => BinocularsConfig -> m [Path Abs File]
files c = do
  dir <- case c ^. binocularsInputNexusdir of
          Nothing  -> getCurrentDir
          (Just d) -> pure d
  (_, fs) <- listDir dir
  if null fs
  then throwM (NoFilesInTheGivenDirectory dir)
  else do
    let fs' = filter isHdf5 fs
    if null fs'
    then throwM (NoDataFilesInTheGivenDirectory dir)
    else case c ^. binocularsInputInputRange of
           Just r  -> do
             let tmpl = maybe "%05d" (unpack . unInputTmpl) (c ^. binocularsInputTmpl)
             let fs'' = filter (isInConfigRange tmpl r) fs'
             if null fs''
             then throwM (NoFilesInRangeInTheGivenDirectory dir r)
             else return fs''
           Nothing -> return fs'
    where
      isHdf5 :: Path Abs File -> Bool
      isHdf5 p = case (fileExtension p :: Maybe [Char]) of
                   Nothing    -> False
                   (Just ext) -> ext `elem` [".h5", ".nxs"]

      matchIndex :: Path Rel File -> String -> Int -> Bool
      matchIndex p tmpl n = printf tmpl n `isInfixOf` toFilePath p

      isInInputRange :: Path Rel File -> String -> InputRange -> Bool
      isInInputRange p tmpl (InputRangeSingle i) = any (matchIndex p tmpl) [i]
      isInInputRange p tmpl (InputRangeFromTo from to) = any (matchIndex p tmpl) [from..to]

      isInConfigRange :: String -> ConfigRange -> Path Abs File -> Bool
      isInConfigRange _ (ConfigRange []) _    = True
      isInConfigRange tmpl (ConfigRange rs) p = any (isInInputRange (filename p) tmpl) rs



replace' :: Int -> Int -> DestinationTmpl -> FilePath
replace' f t = unpack . replace "{last}" (pack . show $ t) . replace "{first}" (pack . show $ f) . unDestinationTmpl

destination' :: ConfigRange -> DestinationTmpl -> FilePath
destination' (ConfigRange rs) = replace' from to
  where
    (from,to) = hull rs

    froms :: [Int]
    froms = [ case r of
                (InputRangeSingle f)   -> f
                (InputRangeFromTo f _) -> f
            | r <- rs]

    tos :: [Int]
    tos = [ case r of
              (InputRangeSingle f)   -> f
              (InputRangeFromTo _ t) -> t
          | r <- rs]

    hull :: [InputRange] -> (Int, Int)
    hull [] = (0, 0)
    hull _  = (minimum froms, maximum tos)

combineWithCmdLineArgs :: BinocularsConfig -> Maybe ConfigRange -> BinocularsConfig
combineWithCmdLineArgs c mr = case mr of
                                Nothing  -> c
                                (Just _) -> c{_binocularsInputInputRange = mr}

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

getMask :: (MonadThrow m, MonadIO m) => BinocularsConfig -> Detector a DIM2 -> m (Maybe Mask)
getMask c d = case _binocularsInputMaskmatrix c of
                Nothing          -> return Nothing
                (Just "default") -> Just <$> getDetectorDefaultMask d
                (Just fname)     -> Just <$> getDetectorMask d fname

getResolution :: MonadThrow m => BinocularsConfig -> Int -> m [Double]
getResolution c n
  | Data.List.length res == 1 = return $ replicate n (head res)
  | Data.List.length res == n = return res
  | otherwise = throwM $ ResolutionNotCompatibleWithProjectionNbOfCoordinates res n
  where
    res = _binocularsProjectionResolution c

newtype ConfigContent = ConfigContent Text

readConfig :: Maybe FilePath -> IO ConfigContent
readConfig mf = do
  cfg <- readFile =<< case mf of
                       Nothing  -> getDataFileName "data/test/config_manip1.cfg"
                       (Just f) -> pure f
  return $ ConfigContent $ unlines $ [fixHeader l | l <- lines cfg]
    where
      fixHeader :: Text -> Text
      fixHeader l = case findIndex (== ']' ) l of
        Nothing  -> l
        (Just n) -> take (n + 1) l

getConfig' :: ConfigContent -> Either String BinocularsConfig
getConfig' (ConfigContent cfg) = do
  let r = parseIni cfg (ini binocularsConfigDefault binocularsConfigSpec)
  mapRight getIniValue r

getConfig :: Maybe FilePath -> IO (Either String BinocularsConfig)
getConfig mf = getConfig' <$> readConfig mf

new :: (MonadIO m, MonadLogger m, MonadThrow m) => Maybe FilePath -> m ()
new mf = do
  cwd <- case mf of
          (Just f) -> parseAbsDir f
          Nothing  -> getCurrentDir
  let conf = binocularsConfigDefault {_binocularsInputNexusdir = Just cwd}
  liftIO $ putStr $ serializeIni (ini conf binocularsConfigSpec)

update :: (MonadIO m, MonadLogger m, MonadThrow m) => FilePath -> m ()
update f = liftIO $ do
  (ConfigContent cfg) <- readConfig (Just f)
  let eini = parseIni cfg (ini binocularsConfigDefault binocularsConfigSpec)
  putStr $ case eini of
             Left s  -> pack s
             Right v -> serializeIni (ini (getIniValue v) binocularsConfigSpec)
