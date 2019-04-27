{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Script
    ( Gnuplot
    , Py2
    , Sh
    , Script(..)
    , run
    , scriptRun
    , scriptSave )
    where

import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Text.IO (writeFile)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.Exit ( ExitCode ( ExitSuccess ) )
import System.FilePath ( (<.>), takeDirectory)
import System.Posix.Files (accessModes, groupModes, ownerModes, setFileMode)
import System.Posix.Types (FileMode)
import System.Process ( rawSystem ) -- callProcess for futur

import Paths_hkl (getDataFileName)

type Profile = Bool

data Gnuplot
data Py2
data Sh

data Script a where
  Py2Script ∷ (Text, FilePath) → Script Py2
  ScriptGnuplot ∷ (Text, FilePath) → Script Gnuplot
  ScriptSh ∷ (Text, FilePath) → Script Sh

scriptSave' ∷ Text → FilePath → FileMode → IO ()
scriptSave' c f m = do
    createDirectoryIfMissing True (takeDirectory f)
    Data.Text.IO.writeFile f c
    setFileMode f m
    print $ "--> created : " ++ f

scriptSave ∷ Script a → IO ()
scriptSave (Py2Script (c, f)) = scriptSave' c f (ownerModes .|. groupModes)
scriptSave (ScriptGnuplot (c, f)) = scriptSave' c f accessModes
scriptSave (ScriptSh (c, f)) = scriptSave' c f (ownerModes .|. groupModes)

scriptRun' ∷ FilePath → String → [String] → Bool → IO ExitCode
scriptRun' f prog args d
    | d = withCurrentDirectory directory go
    | otherwise = go
  where
      go :: IO ExitCode
      go = rawSystem prog args

      directory :: FilePath
      directory = takeDirectory f

scriptRun ∷ Script a → Bool → IO ExitCode
scriptRun (Py2Script (_, p)) d = do
  ExitSuccess ← scriptRun' p "python" args d
  when p' ( do
               gprof2dot ← getDataFileName "data/gprof2dot.py"
               ExitSuccess ← rawSystem gprof2dot ["-f", "pstats", stats, "-o", stats <.> "dot"]
               ExitSuccess ← rawSystem dot ["-Tsvg", "-o", stats <.> "svg", stats <.> "dot"]
               return ()
          )
  return ExitSuccess
    where
      -- BEWARE once actived the profiling multiply by two the computing time.
      p' ∷ Profile
      p' = True

      dot ∷ String
      dot = "dot"

      stats ∷ String
      stats = p <.> "pstats"

      args :: [String]
      args
        | p' = ["-m" , "cProfile", "-o", stats, p]
        | otherwise = [p]
scriptRun (ScriptGnuplot (_, p)) d = scriptRun' p "gnuplot" [p] d
scriptRun (ScriptSh (_, p)) d = scriptRun' p p [] d

run ∷ Script a → Bool → IO ExitCode
run s b = do
  scriptSave s
  scriptRun s b
