{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Flat
       ( Flat(..)
       , Npy
       , computeFlat )
       where

import Data.List (intercalate)
import Data.Text (unlines, pack)
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath.Posix (replaceExtension)
import Prelude hiding (unlines)

import Hkl.DataSource
import Hkl.Nxs
import Hkl.Script

data Npy

data Flat a where
  FlatNpy ∷ FilePath → Flat Npy
deriving instance (Show) (Flat a)

scriptPy2Flat ∷ [Nxs XrdFlat] → FilePath → Script Py2
scriptPy2Flat ns output = Py2Script (script, scriptName)
    where
      script = unlines $
               map pack ["#!/bin/env python"
                        , ""
                        , "import numpy"
                        , "from h5py import File"
                        , "from pyFAI import load"
                        , ""
                        , "NEXUSFILES = [" ++ intercalate ",\n" (map show nxs') ++ "]"
                        , "IMAGEPATHS = [" ++ intercalate ",\n" (map show hpaths) ++ "]"
                        , "OUTPUT = " ++ show output
                        , ""
                        , "flat = None"
                        , "n = None"
                        , "with File(NEXUSFILES[0], mode='r') as f:"
                        , "    imgs = f[IMAGEPATHS[0]]"
                        , "    flat = numpy.sum(imgs[:], axis=0)"
                        , "    n = imgs.shape[0]"
                        , "for idx, (nxs, h5path) in enumerate(zip(NEXUSFILES[1:], IMAGEPATHS[1:])):"
                        , "    with File(nxs, mode='r') as f:"
                        , "        imgs = f[h5path]"
                        , "        flat += numpy.sum(imgs[:], axis=0)"
                        , "        n += imgs.shape[0]"
                        , "numpy.save(OUTPUT, flat / n)"
                        ]
      nxs' ∷ [String]
      nxs' = [f | (Nxs f _) ← ns]

      hpaths ∷ [String]
      hpaths = [h | (Nxs _ (XrdFlatH5Path (DataItemH5 h _))) ← ns]

      scriptName ∷ FilePath
      scriptName = output `replaceExtension` "py"

computeFlat ∷ [Nxs XrdFlat] → FilePath → IO (Flat Npy)
computeFlat ns o = do
  -- create the python script.
  let script = scriptPy2Flat ns o
  scriptSave script
  -- execute this script.
  ExitSuccess ← scriptRun script False
  -- return the filepath of the generated file.
  return (FlatNpy o)
