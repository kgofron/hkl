{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Hkl.Exception where

import           Control.Exception     (Exception, SomeException)
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import           Path                  (Abs, Dir, Path)

data HklDetectorException = MaskShapeNotcompatible Text
                          | NoDefaultMask
    deriving (Show)
instance Exception HklDetectorException

data HklBinocularsException
    = WrongAttenuation Text Int Double
    deriving (Show)
instance Exception HklBinocularsException

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsQCustomException


data HklBinocularsConfigException = NoDataFilesUnderTheGivenDirectory (Path Abs Dir)
                                  | ResolutionNotCompatibleWithProjectionNbOfCoordinates [Double] Int
    deriving (Show)

instance Exception HklBinocularsConfigException

data HklH5Exception
  = CanNotFindDatasetWithAttributContent ByteString ByteString
  | CanNotOpenDataset ByteString
  | CanNotOpenGroup ByteString
  | CanNotOpenGroupAt ByteString Int
  | CanNotOpenFile ByteString
  | CanNotOpenH5Or HklH5Exception HklH5Exception
  | ContainNanValue
  deriving (Show)

instance Exception HklH5Exception

data HklDataSourceException
  = CanNotOpenDataSource'Double'Or SomeException SomeException
  deriving (Show)

instance Exception HklDataSourceException
