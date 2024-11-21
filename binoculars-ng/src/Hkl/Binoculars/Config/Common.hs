{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2024 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config.Common
    ( elemFDef
    , elemFDef'
    , elemFMbDef
    , elemFMbDef'
    , eitherF
    , parse'
    , parseFDef
    , parseMb
    , parseMbDef
    ) where

import           Control.Applicative   ((<|>))
import           Data.Ini.Config       (fieldMbOf, parseIniFile, section)
import           Data.Ini.Config.Bidir (FieldValue (..))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)

import           Hkl.Binoculars.Config


parse' :: HasFieldValue b => Text -> Text -> Text -> Either String (Maybe b)
parse' c s f = parseIniFile c $ section s (fieldMbOf f auto')

eitherF :: (t1 -> p) -> Either t1 t2 -> (t2 -> p) -> p
eitherF fa (Left a)  _ = fa a
eitherF _ (Right b) fb = fb b

parseF :: HasFieldValue b
       => Text -> Text -> Text -> (Maybe b -> Either String r) -> Either String r
parseF c s f = eitherF error (parse' c s f)

parseFDef :: HasFieldValue p
          => Text -> Text -> Text -> p -> Either String p
parseFDef c s f def = parseF c s f $ \case
  Nothing -> Right def
  Just b  -> Right b

parseMb ::  HasFieldValue r
        => Text -> Text -> Text -> Either String (Maybe r)
parseMb c s f = eitherF error (parse' c s f) pure

parseMbDef :: HasFieldValue r
           => Text -> Text -> Text -> Maybe r -> Maybe r
parseMbDef c s f def = eitherF error (parse' c s f) (<|> def)

elemFDef' :: HasFieldComment w => Text -> (v -> w) -> v -> v -> [(Text, Text)]
elemFDef' k f v d = elemFDef k f v d (fieldComment (f v))


elemContent :: Text -> Text -> [Text] -> [Text]
elemContent k def cs =
  [ ""
  , k <> ":"
  , ""
  ]
  <> cs
  <> [ ""
     , "default value: `" <> def <> "`"
     , ""
     , "uncomment and edit the next line if you want to modify the value"
     ]

elemFDef ::  HasFieldValue w => Text -> (v -> w) -> v -> v -> [Text] -> [(Text, Text)]
elemFDef k f v d cs = [ ("#", l) | l <- ls ] <> [(k,  fvEmit fieldvalue (f v))]
  where
    ls :: [Text]
    ls = elemContent k defv cs

    defv :: Text
    defv = fvEmit fieldvalue (f d)

elemFMbDef' ::  HasFieldComment w => Text -> (v -> Maybe w) -> v -> v -> [(Text, Text)]
elemFMbDef' k f v d = elemFMbDef k f v d (fieldComment (fromMaybe undefined (f v)))

elemFMbDef ::  HasFieldValue w => Text -> (v -> Maybe w) -> v -> v -> [Text] -> [(Text, Text)]
elemFMbDef k f v d cs = [ ("#", l) | l <- ls ] <> maybe [("# " <> k, "")] (\w -> [(k, fvEmit fieldvalue w)]) (f v)
  where
    ls :: [Text]
    ls = elemContent k defv cs

    defv :: Text
    defv = case f d of
             Nothing -> "<not set>"
             Just w  -> fvEmit fieldvalue w
