{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Utils
    ( hasContent )
    where

import Data.Text (Text)
import Data.Text.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

hasContent ∷ FilePath → Text → IO ()
hasContent f c = do
    createDirectoryIfMissing True (takeDirectory f)
    Data.Text.IO.writeFile f c
    print $ "--> created : " ++ f
