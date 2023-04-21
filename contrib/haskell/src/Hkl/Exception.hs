{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Hkl.Exception where

import           Control.Exception     (Exception (..))
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import           Data.Typeable         (Typeable)
import           Path                  (Abs, Dir, Path)

data HklDetectorException = MaskShapeNotcompatible Text
                          | NoDefaultMask
    deriving (Show, Typeable)
instance Exception HklDetectorException

data HklBinocularsException
    = WrongAttenuation Text Int Double
    deriving (Show, Typeable)
instance Exception HklBinocularsException

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show, Typeable)

instance Exception HklBinocularsProjectionsQCustomException


data HklBinocularsConfigException = NoDataFilesUnderTheGivenDirectory (Path Abs Dir)
                                  | ResolutionNotCompatibleWithProjectionNbOfCoordinates [Double] Int
    deriving (Show, Typeable)

instance Exception HklBinocularsConfigException


data HklDataSourceException
  = CanNotOpenDataSource'Double'Or HklDataSourceException HklDataSourceException
  | HklDataSourceException'HklH5Exception HklH5Exception
  deriving (Show, Typeable)

instance Exception HklDataSourceException


data HklH5Exception
  = CanNotFindDatasetWithAttributContent ByteString ByteString
  | CanNotOpenDataset ByteString
  | CanNotOpenGroup ByteString
  | CanNotOpenGroupAt ByteString Int
  | CanNotOpenFile ByteString
  | CanNotOpenH5Or HklH5Exception HklH5Exception
  | ContainNanValue
  deriving (Show, Typeable)

instance Exception HklH5Exception where
  toException = toException . HklDataSourceException'HklH5Exception -- cast up through the Parent type
  fromException se =
    case fromException se of
      Just (HklDataSourceException'HklH5Exception c) -> Just c
      _                                              -> Nothing
