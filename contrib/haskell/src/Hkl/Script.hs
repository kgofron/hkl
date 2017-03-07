{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Script
    ( Gnuplot
    , Py2
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

type Profile = Bool

data Gnuplot
data Py2


data Script a where
  Py2Script ∷ (Text, FilePath) → Script Py2
  ScriptGnuplot ∷ (Text, FilePath) → Script Gnuplot

scriptSave' ∷ Text → FilePath → IO ()
scriptSave' c f = do
    createDirectoryIfMissing True (takeDirectory f)
    writeFile f c
    print $ "--> created : " ++ f

scriptSave ∷ Script a → IO ()
scriptSave (Py2Script (c, f)) = scriptSave' c f
scriptSave (ScriptGnuplot (c, f)) = scriptSave' c f

scriptRun' ∷ FilePath → String → [String] → Bool → IO ExitCode
scriptRun' f prog args d
    | d == True = withCurrentDirectory directory go
    | otherwise = go
  where
      go :: IO ExitCode
      go = rawSystem prog args

      directory :: FilePath
      directory = takeDirectory f

scriptRun ∷ Script a → Bool → IO ExitCode
scriptRun (Py2Script (_, p)) d = scriptRun' p "python" args d
    where
      p' ∷ Profile
      p' = False

      args :: [String]
      args
        | p' == True = ["-m" , "cProfile", p]
        | otherwise = [p]
scriptRun (ScriptGnuplot (_, p)) d = scriptRun' p "gnuplot" [p] d

run ∷ Script a → Bool → IO ExitCode
run s b = do
  scriptSave s
  scriptRun s b
