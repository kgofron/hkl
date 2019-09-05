{-# LANGUAGE GADTs #-}
module Hkl.Pipes
    ( withFileP
    , withDatasetP
    , withDataspaceP
    , withHdf5PathP
    )
    where

import           Bindings.HDF5.Core      (Location)
import           Bindings.HDF5.Dataset   (Dataset, closeDataset, openDataset)
import           Bindings.HDF5.Dataspace (Dataspace, closeDataspace)
import           Bindings.HDF5.File      (File, closeFile)
import           Bindings.HDF5.Group     (Group, closeGroup, openGroup)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Pipes.Safe              (MonadSafe, bracket)

import           Hkl.H5

-- | Deal with hdf5 object in a safe way

bracket' :: MonadSafe m => (a -> IO ()) -> IO a -> (a -> m r) -> m r
bracket' r a = bracket (liftIO a) (liftIO . r)

withFileP :: MonadSafe m => IO File -> (File -> m r) -> m r
withFileP = bracket' closeFile

withGroupP :: MonadSafe m => IO Group -> (Group -> m r) -> m r
withGroupP = bracket' closeGroup

withDatasetP :: MonadSafe m => IO Dataset -> (Dataset -> m r) -> m r
withDatasetP = bracket' closeDataset

withDataspaceP :: MonadSafe m => IO Dataspace -> (Dataspace -> m r) -> m r
withDataspaceP = bracket' closeDataspace

withHdf5PathP :: MonadSafe m => File -> Hdf5Path sh e -> (Dataset -> m r) -> m r
withHdf5PathP f path g = go f path g
  where
    go ::  (MonadSafe m, Location l) => l -> Hdf5Path sh e -> (Dataset -> m r) -> m r
    go l (H5RootPath subpath) g' = go l subpath g'
    go l (H5GroupPath n subpath) g' = withGroupP (openGroup l n Nothing) $ \g'' -> go g'' subpath g'
    go l (H5DatasetPath n) g' = withDatasetP (openDataset l n Nothing) g'
