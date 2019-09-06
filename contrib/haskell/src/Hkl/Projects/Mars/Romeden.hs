{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Hkl.Projects.Mars.Romeden
       ( romeden ) where

import           Codec.Picture        (saveTiffImage)
import           Control.Arrow        ((&&&))
import           System.FilePath      ((</>))
import           System.FilePath.Glob (compile, globDir1)

import           Prelude              hiding (concat, lookup, readFile,
                                       writeFile)

import           Hkl

-- | TODO
-- ne pas planter lorsque l'image est manquante dans une nx entry.

project ∷ FilePath
-- project = "/nfs/ruche-mars/mars-soleil/com-mars/2017_Run2/comisioning_microfaisceau"
-- project = "/home/experiences/instrumentation/picca"
project = "/media/picca/Transcend/ROMEDENNE"

h5path ∷ NxEntry → DataFrameH5Path XrdFlat
h5path nxentry =
  XrdFlatH5Path
  (DataItemH5 (nxentry </> image) StrictDims)
  where
    image ∷ H5Path
    image = "image#0/data"

saveAsTiff' ∷ (Nxs XrdFlat, FilePath) → IO ()
saveAsTiff' (n, o) = saveTiffImage o =<< toTiff n

saveAsTiff ∷ (NxEntry -> DataFrameH5Path XrdFlat) → FilePath → IO ()
saveAsTiff h5path' n = mapM_ (saveAsTiff' . (nxs &&& out)) =<< nxEntries n
  where
    nxs ∷ FilePath → Nxs XrdFlat
    nxs nx = mkNxs (project </> n) nx h5path'

    out ∷ FilePath → FilePath
    out nx = (project </> n) ++  nx ++ ".tiff"

-- | Main

romeden ∷ IO ()
romeden = mapM_ (saveAsTiff h5path) =<< globDir1 (compile "*.nxs") project
