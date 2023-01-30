{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config.Common
    ( BinocularsConfig'Common(..)
    , defaultBinocularsConfig'Common
    , elemF
    , elemFMb
    , parseBinocularsConfig'Common
    , parseFDef
    , parseMb
    , parseMbDef
    ) where

import           Control.Applicative               ((<|>))
import           Control.Monad.Catch               (Exception, MonadThrow)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger)
import           Data.Array.Repa.Index             (DIM2)
import           Data.Ini.Config                   (fieldMbOf, parseIniFile,
                                                    section)
import           Data.Ini.Config.Bidir             (FieldValue (..))
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import           GHC.Conc                          (getNumCapabilities,
                                                    getNumProcessors)
import           GHC.Generics                      (Generic)
import           Generic.Random                    (genericArbitraryU)
import           Numeric.Interval                  (empty)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Test.QuickCheck                   (Arbitrary (..))

import           Hkl.Binoculars.Config
import           Hkl.Detector
import           Hkl.Orphan                        ()

data BinocularsConfig'Common
  = BinocularsConfig'Common
    { binocularsConfig'Common'NCores                 :: NCores
    , binocularsConfig'Common'Destination            :: DestinationTmpl
    , binocularsConfig'Common'Overwrite              :: Bool
    , binocularsConfig'Common'InputType              :: InputType
    , binocularsConfig'Common'Nexusdir               :: Maybe (Path Abs Dir)
    , binocularsConfig'Common'Tmpl                   :: Maybe InputTmpl
    , binocularsConfig'Common'InputRange             :: ConfigRange
    , binocularsConfig'Common'Detector               :: Detector Hkl DIM2
    , binocularsConfig'Common'Centralpixel           :: (Int, Int)
    , binocularsConfig'Common'Sdd                    :: Meter
    , binocularsConfig'Common'Detrot                 :: Degree
    , binocularsConfig'Common'AttenuationCoefficient :: Maybe Double
    , binocularsConfig'Common'AttenuationMax         :: Maybe Float
    , binocularsConfig'Common'Maskmatrix             :: Maybe MaskLocation
    , binocularsConfig'Common'Wavelength             :: Maybe Angstrom
    , binocularsConfig'Common'ImageSumMax            :: Maybe Double
    } deriving (Eq, Show, Generic)

defaultBinocularsConfig'Common :: BinocularsConfig'Common
defaultBinocularsConfig'Common
  = BinocularsConfig'Common
    { binocularsConfig'Common'NCores = NCores 4
    , binocularsConfig'Common'Destination = DestinationTmpl "."
    , binocularsConfig'Common'Overwrite = False
    , binocularsConfig'Common'InputType = SixsFlyScanUhv
    , binocularsConfig'Common'Nexusdir = Nothing
    , binocularsConfig'Common'Tmpl = Nothing
    , binocularsConfig'Common'InputRange  = ConfigRange (InputRange Numeric.Interval.empty :| [])
    , binocularsConfig'Common'Detector = defaultDetector
    , binocularsConfig'Common'Centralpixel = (0, 0)
    , binocularsConfig'Common'Sdd = Meter (1 *~ meter)
    , binocularsConfig'Common'Detrot = Degree (0 *~ degree)
    , binocularsConfig'Common'AttenuationCoefficient = Nothing
    , binocularsConfig'Common'AttenuationMax = Nothing
    , binocularsConfig'Common'Maskmatrix = Nothing
    , binocularsConfig'Common'Wavelength = Nothing
    , binocularsConfig'Common'ImageSumMax = Nothing
    }

instance Arbitrary BinocularsConfig'Common where
  arbitrary = genericArbitraryU

parseBinocularsConfig'Common :: (MonadThrow m, MonadLogger m, MonadIO m)
                             => Text -> Maybe ConfigRange -> m (Either String BinocularsConfig'Common)
parseBinocularsConfig'Common cfg mr
  = do
    -- section dispatcher
    ncores <- eitherF error (parse' cfg "dispatcher" "ncores") $ \mb -> do
      ncapmax <- liftIO getNumCapabilities
      ncoresmax <- liftIO getNumProcessors
      let ns = case mb of
            Nothing -> [ncapmax, ncoresmax - 1]
            Just b  -> [b, ncapmax, ncoresmax -1]
      pure $ NCores (minimum ns)
    destination <- parseFDef cfg "dispatcher" "destination" (binocularsConfig'Common'Destination defaultBinocularsConfig'Common)
    overwrite <- parseFDef cfg "dispatcher" "overwrite" (binocularsConfig'Common'Overwrite defaultBinocularsConfig'Common)

    -- section input
    inputtype <- parseFDef cfg "input" "type" (binocularsConfig'Common'InputType defaultBinocularsConfig'Common)
    nexusdir <- parseMb cfg "input" "nexusdir"
    inputtmpl <- parseMb cfg "input" "inputtmpl"
    inputrange <- eitherF error (parse' cfg "dispatcher" "ncores") $ \mb -> do
      let mr' = mr <|> mb
      case mr' of
        Nothing -> error "please provide an input range either in the config file with the \"inputrange\" key under the \"input\" section, or on the command line"
        (Just r) -> pure r
    detector <- parseFDef cfg "input" "detector" (binocularsConfig'Common'Detector defaultBinocularsConfig'Common)
    centralpixel <- eitherF error (parse' cfg "input" "centralpixel") $ \mc -> do
      case mc of
        Nothing -> pure (binocularsConfig'Common'Centralpixel defaultBinocularsConfig'Common)
        Just c -> if c `inDetector` detector
                 then pure c
                 else error $ "The central pixel " <> show c <> " is not compatible with the detector"
    sdd <- parseFDef cfg "input" "sdd" (binocularsConfig'Common'Sdd defaultBinocularsConfig'Common)
    detrot <- parseFDef cfg "input" "detrot" (binocularsConfig'Common'Detrot defaultBinocularsConfig'Common)
    attenuation_coefficient <- parseMb cfg "input" "attenuation_coefficient"
    attenuation_max <- parseMb cfg "input" "attenuation_max"
    maskmatrix <-parseMb cfg "input" "maskmatrix"
    wavelength <- parseMb cfg "input" "wavelength"
    image_sum_max <- parseMb cfg "input" "image_sum_max"

    -- customize a bunch of parameters

    pure $ Right $ BinocularsConfig'Common ncores destination overwrite inputtype nexusdir inputtmpl inputrange detector centralpixel sdd detrot attenuation_coefficient attenuation_max maskmatrix wavelength image_sum_max

parse' :: HasFieldValue b => Text -> Text -> Text -> Either String (Maybe b)
parse' c s f = parseIniFile c $ section s (fieldMbOf f auto')

eitherF :: (t1 -> p) -> Either t1 t2 -> (t2 -> p) -> p
eitherF fa (Left a)  _ = fa a
eitherF _ (Right b) fb = fb b

parseF :: (MonadLogger m, MonadIO m, HasFieldValue b)
       => Text -> Text -> Text -> (Maybe b -> m r) -> m r
parseF c s f = eitherF error (parse' c s f)

parseFDef :: (MonadLogger m, MonadIO m, HasFieldValue p)
          => Text -> Text -> Text -> p -> m p
parseFDef c s f def = parseF c s f (pure . fromMaybe def)

parseMb ::  (MonadLogger m, MonadIO m, HasFieldValue r)
        => Text -> Text -> Text -> m (Maybe r)
parseMb c s f = eitherF error (parse' c s f) pure

parseMbDef :: (MonadLogger m, MonadIO m, HasFieldValue r)
           => Text -> Text -> Text -> Maybe r -> m (Maybe r)
parseMbDef c s f def = eitherF error (parse' c s f) (\mb -> pure $ mb <|> def)

elemF :: HasFieldValue a => Text -> a -> [(Text, Text)]
elemF k v = [(k,  fvEmit fieldvalue v)]

elemFMb :: HasFieldValue a => Text -> Maybe a -> [(Text, Text)]
elemFMb k = maybe [("# " <> k, "")] (elemF k)


------------------
-- Input Path's --
------------------

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsQCustomException
