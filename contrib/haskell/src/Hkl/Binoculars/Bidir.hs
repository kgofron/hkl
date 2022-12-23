{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |
-- Module     : Data.Ini.Config.Bidir
-- Copyright  : (c) Getty Ritter, 2017, 2022
-- License    : BSD
-- Maintainer : Getty Ritter <config-ini@infinitenegativeutility.com>
-- Stability  : experimental
--
-- This module presents an alternate API for parsing INI files.  Unlike
-- the standard API, it is bidirectional: the same declarative structure
-- can be used to parse an INI file to a value, serialize an INI file
-- from a value, or even /update/ an INI file by comparing it against a
-- value and serializing in a way that minimizes the differences between
-- revisions of the file.
--
-- This API does make some extra assumptions about your configuration
-- type and the way you interact with it: in particular, it assumes that
-- you have lenses for all the fields you're parsing and that you have
-- some kind of sensible default value of that configuration
-- type. Instead of providing combinators which can extract and parse a
-- field of an INI file into a value, the bidirectional API allows you to
-- declaratively associate a lens into your structure with a field of the
-- INI file.
--
-- Consider the following example INI file:
--
-- > [NETWORK]
-- > host = example.com
-- > port = 7878
-- >
-- > [LOCAL]
-- > user = terry
--
-- We'd like to parse this INI file into a @Config@ type which we've
-- defined like this, using
-- <https://hackage.haskell.org/package/lens lens> or a similar library
-- to provide lenses:
--
-- > data Config = Config
-- >   { _cfHost :: String
-- >   , _cfPort :: Int
-- >   , _cfUser :: Maybe Text
-- >   } deriving (Eq, Show)
-- >
-- > ''makeLenses Config
--
-- We can now define a basic specification of the type @'IniSpec' Config
-- ()@ by using the provided operations to declare our top-level
-- sections, and then within those sections we can associate fields with
-- @Config@ lenses.
--
-- @
-- 'configSpec' :: 'IniSpec' Config ()
-- 'configSpec' = do
--   'section' \"NETWORK\" $ do
--     cfHost '.=' 'field' \"host\" 'string'
--     cfPost '.=' 'field' \"port\" 'number'
--   'sectionOpt' \"LOCAL\" $ do
--     cfUser '.=?' 'field' \"user\" 'text'
-- @
--
-- There are two operators used to associate lenses with fields:
--
-- ['.='] Associates a lens of type @Lens' s a@ with a field description
--        of type @FieldDescription a@. By default, this will raise an
--        error when parsing if the field described is missing, but we
--        can mark it as optional, as we'll see.
--
-- ['.=?'] Associates a lens of type @Lens' s (Maybe a)@ with a field
--         description of type @FieldDescription a@. During parsing, if
--         the value does not appear in an INI file, then the lens will
--         be set to 'Nothing'; similarly, during serializing, if the
--         value is 'Nothing', then the field will not be serialized in
--         the file.
--
-- Each field must include the field's name as well as a 'FieldValue',
-- which describes how to both parse and serialize a value of a given
-- type. Several built-in 'FieldValue' descriptions are provided, but you
-- can always build your own by providing parsing and serialization
-- functions for individual fields.
--
-- We can also provide extra metadata about a field, allowing it to be
-- skipped durin parsing, or to provide an explicit default value, or to
-- include an explanatory comment for that value to be used when we
-- serialize an INI file. These are conventionally applied to the field
-- using the '&' operator:
--
-- @
-- configSpec :: 'IniSpec' Config ()
-- configSpec = do
--   'section' \"NETWORK\" $ do
--     cfHost '.=' 'field' \"host\" 'string'
--                 & 'comment' [\"The desired hostname (optional)\"]
--                 & 'optional'
--     cfPost '.=' 'field' \"port\" 'number'
--                 & 'comment' [\"The port number\"]
--   'sectionOpt' \"LOCAL\" $ do
--     cfUser '.=?' 'field' \"user\" 'text'
-- @
--
-- When we want to use this specification, we need to create a value of
-- type 'Ini', which is an abstract representation of an INI
-- specification. To create an 'Ini' value, we need to use the 'ini'
-- function, which combines the spec with the default version of our
-- configuration value.
--
-- Once we have a value of type 'Ini', we can use it for three basic
-- operations:
--
-- * We can parse a textual INI file with 'parseIni', which will
--   systematically walk the spec and use the provided lens/field
--   associations to create a parsed configuration file. This will give
--   us a new value of type 'Ini' that represents the parsed
--   configuration, and we can extract the actual configuration value
--   with 'getIniValue'.
--
-- * We can update the value contained in an 'Ini' value. If the 'Ini'
--   value is the result of a previous call to 'parseIni', then this
--   update will attempt to retain as much of the incidental structure of
--   the parsed file as it can: for example, it will attempt to retain
--   comments, whitespace, and ordering. The general strategy is to make
--   the resulting INI file "diff-minimal": the diff between the older
--   INI file and the updated INI file should contain as little noise as
--   possible. Small cosmetic choices such as how to treat generated
--   comments are controlled by a configurable 'UpdatePolicy' value.
--
-- * We can serialize an 'Ini' value to a textual INI file. This will
--   produce the specified INI file (either a default fresh INI, or a
--   modified existing INI) as a textual value.
module Hkl.Binoculars.Bidir
  ( -- ** Serializing INI files
    serializeIni',
  )
where

import qualified Data.Foldable          as F
import           Data.Ini.Config.Bidir  (Ini, getRawIni)
import           Data.Ini.Config.Raw
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder


-- | Serialize an INI file to text, complete with any comments which
-- appear in the INI structure, and retaining the aesthetic details
-- which are present in the INI file.
printRawIni' :: RawIni -> Text
printRawIni' = LazyText.toStrict . Builder.toLazyText . F.foldMap build . fromRawIni
  where
    build (_, ini) =
      F.foldMap buildComment (isComments ini) <>
      Builder.singleton '[' <>
      Builder.fromText (isName ini) <>
      Builder.fromString "]\n" <>
      F.foldMap buildKV (isVals ini)
    buildComment BlankLine = Builder.singleton '\n'
    buildComment (CommentLine c txt) =
      Builder.singleton c <> Builder.fromText txt <> Builder.singleton '\n'
    buildKV (_, val) =
      F.foldMap buildComment (vComments val) <>
      -- mempty <>
      (if vValue val == " " then Builder.fromString "# " else mempty) <>
      -- (if vCommentedOut val then Builder.fromString "# " else mempty) <>
      Builder.fromText (vName val) <>
      Builder.singleton (vDelimiter val) <>
      Builder.fromText (vValue val) <>
      Builder.singleton '\n'

serializeIni' :: Ini s -> Text
serializeIni' = printRawIni' . getRawIni
