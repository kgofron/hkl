{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.AzimuthalIntegrator
       ( AIMethod(..)
       ) where

data AIMethod = Numpy | Cython | SplitPixel | Lut | Csr | NoSplitCsr | FullCsr | LutOcl | CsrOcl

instance Show AIMethod where
  show Numpy = "numpy"
  show Cython = "cython"
  show SplitPixel = "splitpixel"
  show Lut = "lut"
  show Csr = "csr"
  show NoSplitCsr = "nosplit_csr"
  show FullCsr = "full_csr"
  show LutOcl = "lut_ocl"
  show CsrOcl = "csr_ocl"
