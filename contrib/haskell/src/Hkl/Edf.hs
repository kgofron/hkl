{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Edf
       ( Edf(..)
       , ExtractEdf(..)
       , edfP
       , edfFromFile
       ) where

import Data.Attoparsec.Text ( Parser
                            , (<?>)
                            , anyChar
                            , double
                            , many1
                            , manyTill
                            , parseOnly
                            , skipSpace
                            , string
                            , takeTill
                            , try
                            )
import Data.ByteString.Char8 (readFile, split)
import Data.Text (Text, words)
import Data.Text.Encoding (decodeUtf8)
import Numeric.Units.Dimensional.Prelude (Length, (*~), nano, meter)

data Edf = Edf { edf'Lambda :: Length Double
               , edf'Motors :: [(Text, Double)]
               }
         deriving (Show)

class ExtractEdf a where
  extractEdf ∷ a → IO ()


edf'LambdaP :: Parser (Length Double)
edf'LambdaP = do
  _ <- manyTill anyChar (try $ string "Lambda = ")
  value <- double
  pure $ value *~ nano meter

edf'MotorsP :: Parser [(Text, Double)]
edf'MotorsP = do
  _ <- manyTill anyChar (try $ string "motor_pos = ")
  vs <- many1 (skipSpace *> double)
  _ <- manyTill anyChar (try $ string "motor_mne = ")
  ns <- takeTill (\c -> c == ';')
  return $ zip (Data.Text.words ns) vs

edfP :: Parser Edf
edfP = Edf
       <$> edf'LambdaP
       <*> edf'MotorsP
         <?> "edfP"

edfFromFile :: FilePath -> IO Edf
edfFromFile filename = do
  content <- Data.ByteString.Char8.readFile filename
  let header = head (split '}' content)
  return $ case parseOnly edfP (decodeUtf8 header) of
    Left _     -> error $ "Can not parse the " ++ filename ++ " edf file"
    Right a -> a

-- main :: IO ()
-- main = do
--   edf <- edfFromFile "/home/picca/test.edf"
--   print edf
--   return ()
