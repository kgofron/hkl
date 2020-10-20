{-# LANGUAGE GADTs #-}
module Hkl.Conduit
    ( withFileP
    , withDatasetP
    , withDataspaceP
    , withHdf5PathP
    )
    where

import           Bindings.HDF5.Core      (Location)
import           Bindings.HDF5.Dataspace (Dataspace, closeDataspace)
import           Bindings.HDF5.Group     (Group, closeGroup, openGroup)
import           Conduit                 (ConduitT, MonadResource, bracketP)
import           Control.Monad.IO.Class  (MonadIO (liftIO))

import           Hkl.H5

--  Deal with hdf5 object in a safe way

bracket' :: MonadResource m => (a -> IO ()) -> IO a -> (a -> ConduitT i o m r) -> ConduitT i o m r
bracket' r a = bracketP (liftIO a) (liftIO . r)

withFileP :: MonadResource m => IO File -> (File -> ConduitT i o m r) -> ConduitT i o m r
withFileP = bracket' closeFile

withGroupP :: MonadResource m => IO Group -> (Group -> ConduitT i o m r) -> ConduitT i o m r
withGroupP = bracket' closeGroup

withGroupAtP :: (Location l, MonadResource m) => l -> Int -> (Group -> ConduitT i o m r) -> ConduitT i o m r
withGroupAtP l i f = do
  es <- liftIO $ nxEntries' l
  withGroupP (openGroup l (es !! i) Nothing) f

withDatasetP :: MonadResource m => IO Dataset -> (Dataset -> ConduitT i o m r) -> ConduitT i o m r
withDatasetP = bracket' closeDataset

withDataspaceP :: MonadResource m => IO Dataspace -> (Dataspace -> ConduitT i o m r) -> ConduitT i o m r
withDataspaceP = bracket' closeDataspace

withHdf5PathP :: (Location l, MonadResource m) => l -> Hdf5Path sh e -> (Dataset -> ConduitT i o m r) -> ConduitT i o m r
withHdf5PathP l (H5RootPath subpath) f = withHdf5PathP l subpath f
withHdf5PathP l (H5GroupPath n subpath) f = withGroupP (openGroup l n Nothing) $ \g -> withHdf5PathP g subpath f
withHdf5PathP l (H5GroupAtPath i subpath) f = withGroupAtP l i $ \g -> withHdf5PathP g subpath f
withHdf5PathP l (H5DatasetPath n) f = withDatasetP (openDataset l n Nothing) f
