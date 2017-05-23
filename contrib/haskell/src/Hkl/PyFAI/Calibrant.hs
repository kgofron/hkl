{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Calibrant
       ( Calibrant(..) ) where

data Calibrant = CeO2

instance Show Calibrant where
  show CeO2 = "CeO2"
