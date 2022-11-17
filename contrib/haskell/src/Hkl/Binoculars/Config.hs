{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
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
    ( Angstrom(..)
    , Attenuation(..)
    , BinocularsPreConfig(..)
    , Config
    , ConfigContent(..)
    , ConfigRange(..)
    , DataPath
    , Degree(..)
    , DestinationTmpl(..)
    , HasFieldValue(..)
    , HasIniConfig(..)
    , InputRange(..)
    , InputTmpl(..)
    , InputType(..)
    , Limits(..)
    , MaskLocation(..)
    , Meter(..)
    , ProjectionType(..)
    , SampleAxis(..)
    , SurfaceOrientation(..)
    , auto
    , configRangeP
    , destination'
    , files
    , getMask
    , getPreConfig
    , getResolution
    , limitsP
    , projectionTypeP
    , readConfig
    ) where


import           Control.Applicative               (many, (<|>))
import           Control.Lens                      (makeLenses)
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.Catch.Pure          (runCatch)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Data.Array.Repa.Index             (DIM2)
import           Data.Attoparsec.Text              (Parser, char, decimal,
                                                    double, parseOnly, satisfy,
                                                    sepBy, sepBy1', takeText)
import           Data.Either.Extra                 (mapLeft, mapRight)
import           Data.Ini.Config.Bidir             (FieldValue (..), IniSpec,
                                                    bool, field, getIniValue,
                                                    ini, listWithSeparator,
                                                    number, parseIni, section,
                                                    text, (.=))
import           Data.List                         (isInfixOf, length)
import           Data.String                       (IsString)
import           Data.Text                         (Text, breakOn, drop, empty,
                                                    findIndex, intercalate,
                                                    length, lines, pack,
                                                    replace, singleton, strip,
                                                    take, takeWhile, toLower,
                                                    unlines, unpack, unwords)
import           Data.Text.IO                      (readFile)
import           Data.Typeable                     (Typeable)
import           GHC.Exts                          (IsList)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Length, degree, meter, (*~),
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
import           Hkl.Lattice
import           Paths_hkl


data HklBinocularsConfigException = NoFilesInTheGivenDirectory (Path Abs Dir)
                                  | NoDataFilesInTheGivenDirectory (Path Abs Dir)
                                  | NoFilesInRangeInTheGivenDirectory (Path Abs Dir) ConfigRange
                                  | ResolutionNotCompatibleWithProjectionNbOfCoordinates [Double] Int
    deriving (Show)
instance Exception HklBinocularsConfigException

newtype Attenuation = Attenuation { unAttenuation :: Double }
  deriving (Eq, Show)

newtype ConfigContent = ConfigContent Text

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
               | SixsFlyScanUhvTest
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

newtype SampleAxis = SampleAxis { unSampleAxis :: Text }
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
                    | AnglesProjection
                    | Angles2Projection

  deriving (Eq, Show)

data Limits = Limits (Maybe Double) (Maybe Double)
  deriving (Eq, Show)

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

------------------
-- HasIniConfig --
------------------

data family Config (a :: ProjectionType)
data family DataPath (a :: ProjectionType)

class HasIniConfig (a :: ProjectionType) where
  defaultConfig :: Config a

  specConfig :: IniSpec (Config a) ()

  overwriteInputRange :: Maybe ConfigRange -> Config a -> Config a

  getConfig ::  Maybe FilePath -> IO (Either String (Config a))
  getConfig mf = getConfig' <$> readConfig mf
    where
      getConfig' :: HasIniConfig a => ConfigContent -> Either String (Config a)
      getConfig'  (ConfigContent cfg) = do
        let r = parseIni cfg (ini defaultConfig specConfig)
        mapRight getIniValue r

-------------------------
-- BinocularsPreConfig --
-------------------------

data BinocularsPreConfig =
  BinocularsPreConfig { _binocularsPreConfigProjectionType :: ProjectionType }
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

newtype Angstrom = Angstrom { unAngstrom :: Length Double }
    deriving (Eq, Show)

instance FromJSON Angstrom where
  parseJSON = fmap (Angstrom . (*~ angstrom)) . parseJSON

instance ToJSON Angstrom where
  toJSON = toJSON . (/~ angstrom) . unAngstrom

newtype Meter = Meter { unMeter :: Length Double }
    deriving (Eq, Show)

newtype MaskLocation = MaskLocation { unMaskLocation :: Text }
    deriving (Eq, Show, IsString)

number' :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number' = Data.Ini.Config.Bidir.number { fvParse = fvParse Data.Ini.Config.Bidir.number . uncomment}

class HasFieldValue a where
  fieldvalue :: FieldValue a

instance HasFieldValue Angstrom where
  fieldvalue = FieldValue { fvParse =  mapRight (Angstrom . (*~ angstrom)) . fvParse auto
                          , fvEmit = \(Angstrom m) -> pack . show . (/~ angstrom) $ m
                          }

instance HasFieldValue Bool where
  fieldvalue = bool

instance HasFieldValue ConfigRange where
  fieldvalue = parsable

instance HasFieldValue Degree where
  fieldvalue = FieldValue { fvParse =  mapRight (Degree . (*~ degree)) . fvParse auto
                          , fvEmit = \(Degree m) -> pack . show . (/~ degree) $ m
                          }

instance HasFieldValue Double where
  fieldvalue = number'

instance HasFieldValue DestinationTmpl where
  fieldvalue = FieldValue { fvParse = Right . DestinationTmpl . uncomment
                          , fvEmit = \(DestinationTmpl t) -> t
                          }

instance  HasFieldValue (Detector Hkl DIM2) where
  fieldvalue = FieldValue
               { fvParse = parseDetector2D . strip . uncomment
               , fvEmit = \(Detector2D _ name _) -> pack name
               }

instance HasFieldValue Float where
  fieldvalue = number'

instance HasFieldValue InputTmpl where
  fieldvalue = FieldValue { fvParse = Right . InputTmpl . uncomment
                          , fvEmit = \(InputTmpl t) -> t
                          }

instance HasFieldValue InputType where
  fieldvalue = FieldValue { fvParse = parse . strip. uncomment, fvEmit = emit }
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
          | toLower t == emit SixsFlyScanUhvTest = Right SixsFlyScanUhvTest
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
      emit SixsFlyScanUhvTest     = "sixs:flyscanuhvtest"
      emit SixsFlyScanUhvUfxc     = "sixs:flyscanuhvufxc"
      emit SixsSbsFixedDetector   = "sixs:sbsfixeddetector"
      emit SixsSbsMedH            = "sixs:sbsmedh"
      emit SixsSbsMedV            = "sixs:sbsmedv"
      emit SixsSbsMedVFixDetector = "sixs:sbsmedvfixdetector"

instance HasFieldValue Int where
  fieldvalue = number'

instance HasFieldValue MaskLocation where
    fieldvalue = FieldValue { fvParse = mapRight MaskLocation . fvParse text
                            , fvEmit = \(MaskLocation m) -> fvEmit text $ m
                            }

instance HasFieldValue Meter where
  fieldvalue = FieldValue { fvParse =  mapRight (Meter . (*~ meter)) . fvParse auto
                          , fvEmit = \(Meter m) -> pack . show . (/~ meter) $ m
                          }

instance HasFieldValue (Path Abs Dir) where
  fieldvalue = FieldValue { fvParse = \t -> mapLeft show (runCatch . parseAbsDir . unpack $ t)
                          , fvEmit = pack . fromAbsDir
                          }

instance HasFieldValue ProjectionType where
  fieldvalue = parsable

instance HasFieldValue SampleAxis where
  fieldvalue = FieldValue { fvParse = Right . SampleAxis . uncomment
                          , fvEmit = \(SampleAxis t) -> t
                          }

instance HasFieldValue SurfaceOrientation where
  fieldvalue = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
    where
      parse ::  Text -> Either String SurfaceOrientation
      parse t
        | t == emit SurfaceOrientationVertical = Right SurfaceOrientationVertical
        | t == emit SurfaceOrientationHorizontal = Right SurfaceOrientationHorizontal
        | otherwise = Left ("Unsupported " ++ unpack t ++ " surface orientation (vertical or horizontal)")

      emit :: SurfaceOrientation -> Text
      emit SurfaceOrientationVertical   = "vertical"
      emit SurfaceOrientationHorizontal = "horizontal"

instance HasFieldValue Text where
  fieldvalue = text

instance HasFieldValue [Double] where
  fieldvalue = listWithSeparator "," auto

instance HasFieldValue [Limits] where
  fieldvalue = parsable

instance HasFieldValue (Int, Int) where
  fieldvalue = pairWithSeparator' number' "," number'


auto :: HasFieldValue a => FieldValue a
auto = fieldvalue

projectionTypeP :: Parser ProjectionType
projectionTypeP = go =<< takeText
  where
    go :: Text -> Parser ProjectionType
    go t
      | toLower t == fieldEmitter QparQperProjection = return QparQperProjection
      | toLower t == fieldEmitter QxQyQzProjection = return QxQyQzProjection
      | toLower t == fieldEmitter HklProjection = return HklProjection
      | toLower t == fieldEmitter AnglesProjection = return AnglesProjection
      | toLower t == fieldEmitter Angles2Projection = return Angles2Projection
      | toLower t == "sixs:anglesprojection" = return AnglesProjection
      | toLower t == "sixs:angles2projection" = return Angles2Projection
      | toLower t == "sixs:qxqyqzprojection" = return QxQyQzProjection
      | toLower t == "sixs:qparqperprojection" = return QparQperProjection
      | toLower t == "sixs:hklprojection" = return HklProjection
      | otherwise = fail ("Unsupported \"" ++ unpack t ++ "\" projection type format")

instance FieldParsable ProjectionType where
  fieldParser = projectionTypeP

  fieldEmitter QparQperProjection = "qparqper"
  fieldEmitter QxQyQzProjection   = "qxqyqz"
  fieldEmitter HklProjection      = "hkl"
  fieldEmitter AnglesProjection   = "angles"
  fieldEmitter Angles2Projection  = "angles2"

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

files :: (MonadThrow m, MonadIO m)
      => Maybe (Path Abs Dir)
      -> Maybe ConfigRange
      -> Maybe InputTmpl
      -> m [Path Abs File]
files md mr mt = do
  dir <- case md of
          Nothing  -> getCurrentDir
          (Just d) -> pure d
  (_, fs) <- listDir dir
  if null fs
  then throwM (NoFilesInTheGivenDirectory dir)
  else do
    let fs' = filter isHdf5 fs
    if null fs'
    then throwM (NoDataFilesInTheGivenDirectory dir)
    else case mr of
           Just r  -> do
             let tmpl = maybe "%05d" (unpack . unInputTmpl) mt
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



replace' :: Int -> Int -> Text -> DestinationTmpl -> FilePath
replace' f t l = unpack
                 . replace "{last}" (pack . show $ t)
                 . replace "{first}" (pack . show $ f)
                 . replace "{limits}" l
                 . unDestinationTmpl

destination' :: ConfigRange -> Maybe [Limits] -> DestinationTmpl -> FilePath
destination' (ConfigRange rs) ml = replace' from to limits
  where
    (from,to) = hull rs

    limits = case ml of
               Nothing   -> "nolimits"
               (Just ls) -> fieldEmitter ls

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

getMask :: (MonadThrow m, MonadIO m) => Maybe MaskLocation -> Detector a DIM2 -> m (Maybe Mask)
getMask ml d = case ml of
                Nothing          -> return Nothing
                (Just "default") -> Just <$> getDetectorDefaultMask d
                (Just fname)     -> Just <$> getDetectorMask d (unMaskLocation fname)

getResolution :: MonadThrow m => [Double] -> Int -> m [Double]
getResolution res n
  | Data.List.length res == 1 = return $ replicate n (head res)
  | Data.List.length res == n = return res
  | otherwise = throwM $ ResolutionNotCompatibleWithProjectionNbOfCoordinates res n

getPreConfig' :: ConfigContent -> Either String BinocularsPreConfig
getPreConfig' (ConfigContent cfg) = do
  let r = parseIni cfg (ini binocularsPreConfigDefault binocularsPreConfigSpec)
  mapRight getIniValue r

getPreConfig :: Maybe FilePath -> IO (Either String BinocularsPreConfig)
getPreConfig mf = getPreConfig' <$> readConfig mf
