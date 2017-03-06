{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Utils
    ( hasContent )
    where

import Data.Text (Text)
import Data.Text.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
-- import System.Process ( rawSystem ) -- callProcess for futur

import Prelude hiding (writeFile)

hasContent ∷ FilePath → Text → IO ()
hasContent f c = do
    createDirectoryIfMissing True (takeDirectory f)
    writeFile f c
    print $ "--> created : " ++ f
