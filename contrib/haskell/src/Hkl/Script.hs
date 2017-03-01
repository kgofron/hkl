{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Script
    ( Py2
    , Script(..)
    , run
    , scriptRun
    , scriptSave )
        where

import Data.Text (Text)
import Data.Text.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.Exit ( ExitCode )
import System.FilePath (takeDirectory)
import System.Process ( rawSystem ) -- callProcess for futur

import Prelude hiding (writeFile)

#if MIN_VERSION_directory(1, 3, 0)
import System.Directory (withCurrentDirectory)
#else
import Control.Exception.Base (bracket)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action
#endif

data Py2

data Script a where
  Py2Script ∷ (Text, FilePath) → Script Py2

scriptSave ∷ Script a → IO ()
scriptSave (Py2Script (c, f)) = do
    createDirectoryIfMissing True (takeDirectory f)
    writeFile f c
    print $ "--> created : " ++ f

scriptRun ∷ Script a → Bool → IO ExitCode
scriptRun (Py2Script (_, p)) d
    | d == True = withCurrentDirectory directory go
    | otherwise = go
    where
      go :: IO ExitCode
      go = rawSystem python args

      python :: String
      python = "/usr/bin/python"

      args :: [String]
      args = [p]

      directory :: FilePath
      directory = takeDirectory p

run ∷ Script a → Bool → IO ExitCode
run s b = do
  scriptSave s
  scriptRun s b
