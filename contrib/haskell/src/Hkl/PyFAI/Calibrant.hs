{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Calibrant
       ( Calibrant(..) ) where

data Calibrant = CeO2 | LaB6

instance Show Calibrant where
  show CeO2 = "CeO2"
  show LaB6 = "LaB6"
