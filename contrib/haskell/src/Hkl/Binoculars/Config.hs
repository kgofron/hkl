{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config
    ( Angstrom(..)
    , Args
    , Attenuation(..)
    , BinocularsPreConfig(..)
    , Capabilities(..)
    , Config
    , ConfigContent(..)
    , ConfigRange(..)
    , DataPath
    , Degree(..)
    , DestinationTmpl(..)
    , FieldEmitter(..)
    , FieldParsable(..)
    , HasFieldValue(..)
    , HasIniConfig(..)
    , InputRange(..)
    , InputTmpl(..)
    , InputType(..)
    , Limits(..)
    , MaskLocation(..)
    , Meter(..)
    , NCores(..)
    , ProjectionType(..)
    , QCustomSubProjection(..)
    , Resolutions(..)
    , RLimits(..)
    , SampleAxis(..)
    , SurfaceOrientation(..)
    , ToIni(..)
    , auto
    , auto'
    , destination'
    , files
    , getCapabilities
    , getMask
    , getPreConfig
    , mergeIni
    , readConfig
    , serializeConfig
    ) where


import           Control.Applicative               (many, (<|>))
import           Control.Lens                      (makeLenses)
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.Catch.Pure          (runCatch)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Attoparsec.Text              (Parser, char, decimal,
                                                    double, parseOnly, satisfy,
                                                    sepBy, signed, takeText)
import           Data.Either.Combinators           (maybeToRight)
import           Data.Either.Extra                 (mapLeft, mapRight)
import           Data.Foldable                     (foldl')
import           Data.HashMap.Strict               (HashMap, unionWith)
import           Data.Hashable                     (Hashable)
import           Data.Ini                          (Ini (..), printIni)
import           Data.Ini.Config.Bidir             (FieldValue (..), IniSpec,
                                                    bool, field, getIniValue,
                                                    ini, listWithSeparator,
                                                    number, parseIni, section,
                                                    text, (.=))
import           Data.List                         (find, isInfixOf, length)
import           Data.List.NonEmpty                (NonEmpty (..), map)
import           Data.String                       (IsString)
import           Data.Text                         (Text, breakOn, drop, empty,
                                                    findIndex, intercalate,
                                                    length, lines, pack,
                                                    replace, singleton, strip,
                                                    take, takeWhile, toLower,
                                                    unlines, unpack, unwords)
import           Data.Text.IO                      (readFile)
import           Data.Typeable                     (Proxy (..), Typeable,
                                                    typeRep)
import           GHC.Conc                          (getNumCapabilities,
                                                    getNumProcessors)
import           GHC.Exts                          (IsList (..))
import           Numeric.Interval                  (Interval, empty, hull, inf,
                                                    singleton, singular, sup,
                                                    (...))
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Length, degree, meter, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, File, Path, Rel,
                                                    fileExtension, filename,
                                                    fromAbsDir, parseAbsDir,
                                                    toFilePath)
import           Path.IO                           (getCurrentDir, walkDirAccum)
import           System.Directory                  (doesPathExist)
import           System.FilePath                   (splitExtensions)
import           Test.QuickCheck                   (Arbitrary (..), elements,
                                                    oneof)
import           Text.Printf                       (printf)

import           Prelude                           hiding (drop, length, lines,
                                                    putStr, readFile, take,
                                                    takeWhile, unlines, unwords)

import           Hkl.Detector
import           Hkl.Lattice
import           Paths_hkl


-- Class FieldEmitter

class FieldEmitter a where
  fieldEmitter :: a -> Text

-- Class FieldParsable

class FieldEmitter a => FieldParsable a where
  fieldParser :: Parser a

-- Class HasFieldValue

class HasFieldValue a where
  fieldvalue :: FieldValue a

auto :: HasFieldValue a => FieldValue a
auto = fieldvalue

auto' :: HasFieldValue a => Text -> Either String a
auto' = fvParse fieldvalue

instance HasFieldValue Bool where
  fieldvalue = bool

instance HasFieldValue Degree where
  fieldvalue = FieldValue
    { fvParse =  mapRight (Degree . (*~ degree)) . fvParse auto
    , fvEmit = \(Degree m) -> pack . show . (/~ degree) $ m
    }

number' :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number' = Data.Ini.Config.Bidir.number
  { fvParse = fvParse Data.Ini.Config.Bidir.number . uncomment}

instance HasFieldValue Double where
  fieldvalue = number'

instance  HasFieldValue (Detector Hkl DIM2) where
  fieldvalue = FieldValue
               { fvParse = parseDetector2D . strip . uncomment
               , fvEmit = \(Detector2D _ name _) -> pack name
               }

instance HasFieldValue Float where
  fieldvalue = number'

instance HasFieldValue Int where
  fieldvalue = number'

instance HasFieldValue (Path Abs Dir) where
  fieldvalue = FieldValue { fvParse = \t -> mapLeft show (runCatch . parseAbsDir . unpack $ t)
                          , fvEmit = pack . fromAbsDir
                          }

instance HasFieldValue Text where
  fieldvalue = text

instance HasFieldValue [Double] where
  fieldvalue = listWithSeparator "," auto

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

instance HasFieldValue (Int, Int) where
  fieldvalue = pairWithSeparator' number' "," number'

-- Class HasIniConfig

data family Config (a :: ProjectionType)
data family DataPath (a :: ProjectionType)
data family Args (a :: ProjectionType)

readConfig :: Maybe FilePath -> IO ConfigContent
readConfig mf = do
  cfg <- readFile =<< case mf of
                       Nothing  -> getDataFileName "data/test/config_manip1.cfg"
                       (Just f) -> pure f
  -- return $ ConfigContent cfg
  return $ ConfigContent $ unlines $ [fixHeader l | l <- lines cfg]
    where
      fixHeader :: Text -> Text
      fixHeader l = case findIndex (== '#' ) l of
        Nothing  -> l
        (Just n) -> take n l

class HasIniConfig (a :: ProjectionType) where

  getConfig :: ConfigContent
            -> Args a
            -> Capabilities
            -> Either String (Config a)

-- Class ToIni

class ToIni a where
  toIni :: a -> Ini

serializeConfig :: ToIni a => a -> Text
serializeConfig = printIni . toIni

mergeHash :: (Eq k, Hashable k, Semigroup v) => HashMap k v -> HashMap k v -> HashMap k v
mergeHash = unionWith f
  where
    f :: Semigroup v => v -> v -> v
    f v1 v2 = v1 <> v2

mergeIni :: Ini -> Ini -> Ini
mergeIni x y = Ini {iniGlobals = mempty, iniSections = iniSections x `mergeHash` iniSections y}

-- Angstrom

newtype Angstrom = Angstrom { unAngstrom :: Length Double }
    deriving (Eq, Show)

instance FromJSON Angstrom where
  parseJSON = fmap (Angstrom . (*~ angstrom)) . parseJSON

instance ToJSON Angstrom where
  toJSON = toJSON . (/~ angstrom) . unAngstrom

instance Arbitrary Angstrom where
  arbitrary = Angstrom . (*~ angstrom) <$> arbitrary

instance HasFieldValue Angstrom where
  fieldvalue = FieldValue
    { fvParse =  mapRight (Angstrom . (*~ angstrom)) . fvParse auto
    , fvEmit = \(Angstrom m) -> pack . show . (/~ angstrom) $ m
    }

-- Attenuation

newtype Attenuation = Attenuation { unAttenuation :: Double }
  deriving (Eq, Show)

-- Capabilities

data Capabilities = Capabilities Int Int
  deriving (Eq, Show)

getCapabilities :: IO Capabilities
getCapabilities = Capabilities
                  <$> getNumCapabilities
                  <*> getNumProcessors

-- ConfigContent

newtype ConfigContent = ConfigContent Text

-- ConfigRange

newtype ConfigRange = ConfigRange (NonEmpty InputRange)
  deriving (Eq, Show, IsList)

instance Arbitrary ConfigRange where
  arbitrary = ConfigRange <$> ((:|) <$> arbitrary <*> arbitrary)

instance FieldEmitter ConfigRange where
  fieldEmitter (ConfigRange is) = unwords . toList $ Data.List.NonEmpty.map fieldEmitter is

instance FieldParsable ConfigRange where
  fieldParser = ConfigRange <$> ((:|)
                                 <$> fieldParser <* many (satisfy isSep)
                                 <*> fieldParser `sepBy` many (satisfy isSep))
    where
      isSep :: Char -> Bool
      isSep c = c == ' ' || c == ','

instance HasFieldValue ConfigRange where
  fieldvalue = parsable

-- DestinationTmpl

newtype DestinationTmpl =
  DestinationTmpl { unDestinationTmpl :: Text }
  deriving (Eq, Show)

instance Arbitrary DestinationTmpl where
  arbitrary = pure $ DestinationTmpl "{first}_{last}.h5"

instance HasFieldValue DestinationTmpl where
  fieldvalue = FieldValue
    { fvParse = Right . DestinationTmpl . uncomment
    , fvEmit = \(DestinationTmpl t) -> t
    }

-- HklBinocularsConfigException

data HklBinocularsConfigException = NoDataFilesUnderTheGivenDirectory (Path Abs Dir)
                                  | ResolutionNotCompatibleWithProjectionNbOfCoordinates [Double] Int
    deriving (Show)

instance Exception HklBinocularsConfigException

-- InputRange

newtype InputRange = InputRange {unInputRange :: Interval Int }
                   deriving (Eq, Show)

instance Arbitrary InputRange where
  arbitrary = InputRange <$> oneof [ Numeric.Interval.singleton <$> arbitrary
                                   , do
                                       f <- arbitrary
                                       t <- arbitrary
                                       pure $ if f < t then f...t else t...f
                                   ]

instance FieldEmitter InputRange where
  fieldEmitter (InputRange i) = pack $ if singular i
                                       then printf "%d" (sup i)
                                       else printf "%d-%d" (inf i) (sup i)

instance FieldParsable InputRange where
  fieldParser = inputRangeFromToP <|> inputRangeP'
    where
      inputRangeFromToP :: Parser InputRange
      inputRangeFromToP =  InputRange
                           <$> ((...)
                                <$> signed decimal <* char '-'
                                <*> signed decimal)

      inputRangeP' :: Parser InputRange
      inputRangeP' = InputRange
                     <$> (Numeric.Interval.singleton <$> signed decimal)

-- InputTmpl

newtype InputTmpl = InputTmpl { unInputTmpl :: Text }
  deriving (Eq, Show)

instance Arbitrary InputTmpl where
  arbitrary = pure $ InputTmpl "inputfiles%04.nxs"

instance HasFieldValue InputTmpl where
  fieldvalue = FieldValue
    { fvParse = Right . InputTmpl . uncomment
    , fvEmit = \(InputTmpl t) -> t
    }

-- InputType

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
               | SixsSbsUhv
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary InputType where
  arbitrary = elements ([minBound .. maxBound] :: [InputType])

instance HasFieldValue InputType where
  fieldvalue = FieldValue { fvParse = parse . strip. uncomment, fvEmit = emit }
    where
      err t =  "Unsupported "
               ++ show (typeRep (Proxy :: Proxy InputType))
               ++ " :" ++ unpack t
               ++ " Supported ones are: "
               ++ unpack (unwords $ Prelude.map emit [minBound..maxBound])

      parse :: Text -> Either String InputType
      parse t = parseEnum (err t) t

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
      emit SixsSbsUhv             = "sixs:sbsuhv"

-- Limits

data Limits = Limits (Maybe Double) (Maybe Double)
  deriving (Eq, Show)

instance Arbitrary Limits where
  arbitrary = Limits <$> arbitrary <*> arbitrary

-- MaskLocation

newtype MaskLocation = MaskLocation { unMaskLocation :: Text }
    deriving (Eq, Show, IsString)

instance Arbitrary MaskLocation where
  arbitrary = pure $ MaskLocation "mask location"

instance HasFieldValue MaskLocation where
  fieldvalue = FieldValue
    { fvParse = mapRight MaskLocation . fvParse text
    , fvEmit = \(MaskLocation m) -> fvEmit text m
    }

-- Meter

newtype Meter = Meter { unMeter :: Length Double }
    deriving (Eq, Show)

instance Arbitrary Meter where
  arbitrary = Meter . (*~ meter) <$> arbitrary

instance HasFieldValue Meter where
  fieldvalue = FieldValue
    { fvParse =  mapRight (Meter . (*~ meter)) . fvParse auto
    , fvEmit = \(Meter m) -> pack . show . (/~ meter) $ m
    }

-- NCores

newtype NCores = NCores { unNCores :: Int }
    deriving (Eq, Show)

instance Arbitrary NCores where
  arbitrary = NCores <$> arbitrary

instance HasFieldValue NCores where
  fieldvalue = FieldValue
    { fvParse =  mapRight NCores . fvParse auto
    , fvEmit = \(NCores m) -> pack . show $ m
    }

-- ProjectionType

data ProjectionType = AnglesProjection
                    | Angles2Projection
                    | HklProjection
                    | QCustomProjection
                    | QIndexProjection
                    | QparQperProjection
                    | QxQyQzProjection

  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary ProjectionType where
  arbitrary = elements ([minBound .. maxBound] :: [ProjectionType])

instance FieldEmitter ProjectionType where
  fieldEmitter AnglesProjection   = "angles"
  fieldEmitter Angles2Projection  = "angles2"
  fieldEmitter HklProjection      = "hkl"
  fieldEmitter QCustomProjection  = "qcustom"
  fieldEmitter QIndexProjection   = "qindex"
  fieldEmitter QparQperProjection = "qparqper"
  fieldEmitter QxQyQzProjection   = "qxqyqz"

instance FieldParsable ProjectionType where
  fieldParser = go =<< takeText
    where
      err t =  "Unsupported "
               ++ show (typeRep (Proxy :: Proxy ProjectionType))
               ++ " :" ++ unpack t
               ++ " Supported ones are: "
               ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: ProjectionType])

      go :: Text -> Parser ProjectionType
      go t
        | toLower t == "sixs:anglesprojection" = pure AnglesProjection
        | toLower t == "sixs:angles2projection" = pure Angles2Projection
        | toLower t == "sixs:qindex" = pure QIndexProjection
        | toLower t == "sixs:qxqyqzprojection" = pure QxQyQzProjection
        | toLower t == "sixs:qparqperprojection" = pure QparQperProjection
        | toLower t == "sixs:hklprojection" = pure HklProjection
      go t = case parseEnum (err t) t of
        Right p   -> pure p
        Left err' -> fail err'

instance HasFieldValue ProjectionType where
  fieldvalue = parsable

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

parseEnum :: (Bounded a, Enum a, HasFieldValue a, Typeable a)
          => String -> Text -> Either String a
parseEnum err t = maybeToRight err (find match [minBound..maxBound])
  where
    match :: HasFieldValue a => a -> Bool
    match i = toLower t == fvEmit fieldvalue i

-- QCustomSubProjection

data QCustomSubProjection = QCustomSubProjection'QxQyQz
                          | QCustomSubProjection'QTthTimestamp
                          | QCustomSubProjection'QIndex
                          | QCustomSubProjection'QparQperTimestamp
                          | QCustomSubProjection'QparQper
                          | QCustomSubProjection'QPhiQx
                          | QCustomSubProjection'QPhiQy
                          | QCustomSubProjection'QPhiQz
                          | QCustomSubProjection'QStereo
                          | QCustomSubProjection'AnglesZaxisOmega
                          | QCustomSubProjection'AnglesZaxisMu
  deriving (Enum, Bounded, Eq, Show)

instance Arbitrary QCustomSubProjection where
  arbitrary = elements ([minBound .. maxBound] :: [QCustomSubProjection])

instance HasFieldValue QCustomSubProjection where
  fieldvalue = FieldValue { fvParse = parse . strip. uncomment, fvEmit = emit }
    where
      parse :: Text -> Either String QCustomSubProjection
      parse t = parseEnum (err t) t

      err t = "Unsupported "
              ++ show (typeRep (Proxy :: Proxy QCustomSubProjection))
              ++ " :" ++ unpack t
              ++ " Supported ones are: "
              ++ unpack (unwords $ Prelude.map emit [minBound..maxBound])

      emit :: QCustomSubProjection -> Text
      emit QCustomSubProjection'QxQyQz            = "qx_qy_qz"
      emit QCustomSubProjection'QTthTimestamp     = "q_tth_timestamp"
      emit QCustomSubProjection'QIndex            = "q_index"
      emit QCustomSubProjection'QparQperTimestamp = "qpar_qper_timestamp"
      emit QCustomSubProjection'QparQper          = "qpar_qper"
      emit QCustomSubProjection'QPhiQx            = "q_phi_qx"
      emit QCustomSubProjection'QPhiQy            = "q_phi_qy"
      emit QCustomSubProjection'QPhiQz            = "q_phi_qz"
      emit QCustomSubProjection'QStereo           = "q_stereo"
      emit QCustomSubProjection'AnglesZaxisOmega  = "angles_zaxis_omega"
      emit QCustomSubProjection'AnglesZaxisMu     = "angles_zaxis_mu"

-- Resolutions

data Resolutions sh where
  Resolutions2 :: Double -> Double -> Resolutions DIM2
  Resolutions3 :: Double -> Double -> Double -> Resolutions DIM3

deriving instance Eq (Resolutions sh)
deriving instance Show (Resolutions sh)

instance Arbitrary (Resolutions DIM2) where
  arbitrary = Resolutions2 <$> arbitrary <*> arbitrary

instance Arbitrary (Resolutions DIM3) where
  arbitrary = Resolutions3 <$> arbitrary <*> arbitrary <*> arbitrary

instance IsList (Resolutions sh) where
  type Item (Resolutions sh) = Double

  toList (Resolutions2 r1 r2)    = [r1, r2]
  toList (Resolutions3 r1 r2 r3) = [r1, r2, r3]

  fromList = undefined

instance HasFieldValue (Resolutions DIM2) where
  fieldvalue = FieldValue { fvParse = parse, fvEmit = emit }
    where
      parse :: Text -> Either String (Resolutions DIM2)
      parse t = do
        rs <- (fvParse $ listWithSeparator "," auto) t
        case Data.List.length rs of
          1 -> Right (Resolutions2 (head rs) (head rs))
          2 -> Right (Resolutions2 (head rs) (rs !! 1))
          _ -> Left "Need one or two resolutions values for this projection"

      emit :: Resolutions DIM2 -> Text
      emit (Resolutions2 r1 r2) = intercalate "," (Prelude.map (pack . show) [r1, r2])

instance HasFieldValue (Resolutions DIM3) where
  fieldvalue = FieldValue { fvParse = parse, fvEmit = emit }
    where
      parse :: Text -> Either String (Resolutions DIM3)
      parse t = do
        rs <- (fvParse $ listWithSeparator "," auto) t
        case Data.List.length rs of
          1 -> Right (Resolutions3 (head rs) (head rs) (head rs))
          3 -> Right (Resolutions3 (head rs) (rs !! 1) (rs !! 2))
          _ -> Left "Need one or three resolutions values for this projection"

      emit :: Resolutions DIM3 -> Text
      emit (Resolutions3 r1 r2 r3) = intercalate "," (Prelude.map (pack . show) [r1, r2, r3])

-- RLimits

data RLimits sh where
  Limits2 :: Limits -> Limits -> RLimits DIM2
  Limits3 :: Limits -> Limits -> Limits -> RLimits DIM3

deriving instance Eq (RLimits sh)
deriving instance Show (RLimits sh)

instance Arbitrary (RLimits DIM2) where
  arbitrary = Limits2 <$> arbitrary <*> arbitrary


instance Arbitrary (RLimits DIM3) where
  arbitrary = Limits3 <$> arbitrary <*> arbitrary <*> arbitrary


limitsP' :: Parser Limits
limitsP' = Limits
           <$> lim <* char ':'
           <*> lim
  where
    lim :: Parser (Maybe Double)
    lim = (Just <$> double) <|> return Nothing

showLimit :: Maybe Double -> Text
showLimit (Just l) = pack $ printf "%f" l
showLimit Nothing  = Data.Text.empty

showLimits :: Limits -> Text
showLimits (Limits f t) = showLimit f <> Data.Text.singleton ':' <> showLimit t

instance (FieldEmitter (RLimits sh)) where
  fieldEmitter ls = Data.Text.singleton '['
                    <> intercalate "," (Prelude.map showLimits (toList ls))
                    <> Data.Text.singleton ']'

instance (FieldParsable (RLimits DIM2)) where
  fieldParser = Limits2
                <$> (char '[' *> limitsP')
                <*> (char ',' *> limitsP' <* char ']')

instance (FieldParsable (RLimits DIM3)) where
  fieldParser = Limits3
                <$> (char '[' *> limitsP')
                <*> (char ',' *> limitsP')
                <*> (char ',' *> limitsP' <* char ']')

instance HasFieldValue (RLimits DIM2) where
  fieldvalue = parsable

instance HasFieldValue (RLimits DIM3) where
  fieldvalue = parsable

instance IsList (RLimits sh) where
  type Item (RLimits sh) = Limits

  toList (Limits2 l1 l2)    = [l1, l2]
  toList (Limits3 l1 l2 l3) = [l1, l2, l3]

  fromList = undefined

-- SampleAxis

newtype SampleAxis = SampleAxis { unSampleAxis :: Text }
  deriving (Eq, Show)

instance HasFieldValue SampleAxis where
  fieldvalue = FieldValue { fvParse = Right . SampleAxis . uncomment
                          , fvEmit = \(SampleAxis t) -> t
                          }

-- SurfaceOrientation

data SurfaceOrientation = SurfaceOrientationVertical
                        | SurfaceOrientationHorizontal
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary SurfaceOrientation where
  arbitrary = elements ([minBound .. maxBound] :: [SurfaceOrientation])

instance HasFieldValue SurfaceOrientation where
  fieldvalue = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
    where
      err t = "Unsupported "
              ++ show (typeRep (Proxy :: Proxy SurfaceOrientation))
              ++ " :" ++ unpack t
              ++ " Supported ones are: "
              ++ unpack (unwords $ Prelude.map emit [minBound..maxBound])

      parse :: Text -> Either String SurfaceOrientation
      parse t = parseEnum (err t) t

      emit :: SurfaceOrientation -> Text
      emit SurfaceOrientationVertical   = "vertical"
      emit SurfaceOrientationHorizontal = "horizontal"

-- BinocularsPreConfig

newtype BinocularsPreConfig =
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


---------------
-- functions --
---------------

destination' :: ProjectionType -> ConfigRange -> Maybe (RLimits a) -> DestinationTmpl -> Bool -> IO FilePath
destination' proj (ConfigRange rs) ml dtmpl overwrite =
  if overwrite
  then pure $ replace' proj interval limits dtmpl Nothing
  else do
    let guess = replace' proj interval limits dtmpl Nothing : Prelude.map (replace' proj interval limits dtmpl . Just) [2..]
    findFirst guess
      where
        findFirst :: [FilePath] -> IO FilePath
        findFirst [] = undefined -- can not append non empty list
        findFirst (x : xs) = do
          exists <- doesPathExist x
          if exists
            then findFirst xs
            else return x

        interval = foldl' hull Numeric.Interval.empty intervals

        limits = case ml of
                   Nothing   -> "nolimits"
                   (Just ls) -> fieldEmitter ls

        intervals = Data.List.NonEmpty.map unInputRange rs

isHdf5 :: Path Abs File -> Bool
isHdf5 p = case (fileExtension p :: Maybe [Char]) of
             Nothing    -> False
             (Just ext) -> ext `elem` [".h5", ".nxs"]

isInConfigRange :: Maybe InputTmpl -> Maybe ConfigRange ->  Path Abs File -> Bool
isInConfigRange mtmpl mr f
  = case mr of
      (Just (ConfigRange rs)) -> do
        let tmpl = maybe "%05d" (unpack . unInputTmpl) mtmpl
        any (isInInputRange (filename f) tmpl) rs
      Nothing -> True
  where
    matchIndex :: Path Rel File -> String -> Int -> Bool
    matchIndex p tmpl n = printf tmpl n `isInfixOf` toFilePath p

    isInInputRange :: Path Rel File -> String -> InputRange -> Bool
    isInInputRange p tmpl (InputRange i) = any (matchIndex p tmpl) [inf i .. sup i]

files :: (MonadThrow m, MonadIO m)
       => Maybe (Path Abs Dir)
       -> Maybe ConfigRange
       -> Maybe InputTmpl
       -> m [Path Abs File]
files md mr mt
  = do
  let filters = [ isHdf5
                , isInConfigRange mt mr
                ]

  dir <- case md of
        Nothing  -> getCurrentDir
        (Just d) -> pure d

  fs <- walkDirAccum Nothing
       (\_root _dirs fs -> return $ filter (\f -> all ($ f) filters) fs)
       dir

  if null fs
    then throwM (NoDataFilesUnderTheGivenDirectory dir)
    else return fs

getMask :: (MonadThrow m, MonadIO m) => Maybe MaskLocation -> Detector Hkl DIM2 -> m (Maybe Mask)
getMask ml d = case ml of
                Nothing          -> return Nothing
                (Just "default") -> Just <$> getDetectorDefaultMask d
                (Just fname)     -> Just <$> getDetectorMask d (unMaskLocation fname)

getPreConfig' :: ConfigContent -> Either String BinocularsPreConfig
getPreConfig' (ConfigContent cfg) = do
  let r = parseIni cfg (ini binocularsPreConfigDefault binocularsPreConfigSpec)
  mapRight getIniValue r

getPreConfig :: Maybe FilePath -> IO (Either String BinocularsPreConfig)
getPreConfig mf = getPreConfig' <$> readConfig mf

addOverwrite :: Maybe Int -> DestinationTmpl -> DestinationTmpl
addOverwrite midx tmpl = case midx of
  Nothing -> tmpl
  Just idx -> let (f, ext) = splitExtensions . unpack . unDestinationTmpl $ tmpl
             in DestinationTmpl (pack $ f <> printf "_%02d" idx <> ext)

replace' :: ProjectionType -> Interval Int -> Text -> DestinationTmpl -> Maybe Int -> FilePath
replace' proj i l dtmpl midx = unpack
                          . replace "{last}" (pack . show . sup $ i)
                          . replace "{first}" (pack . show . inf $ i)
                          . replace "{limits}" l
                          . replace "{projection}" (fieldEmitter proj)
                          . unDestinationTmpl . addOverwrite midx $ dtmpl
