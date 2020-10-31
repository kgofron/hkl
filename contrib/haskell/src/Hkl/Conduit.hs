{-# LANGUAGE GADTs #-}
module Hkl.Conduit
    ( withBytesC
    , withFileC
    , withDatasetC
    , withDataspaceC
    , withHdf5PathC
    )
    where

import           Bindings.HDF5.Core      (Location)
import           Bindings.HDF5.Dataspace (Dataspace, closeDataspace)
import           Bindings.HDF5.Group     (Group, closeGroup, openGroup)
import           Conduit                 (ConduitT, MonadResource, bracketP)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Foreign.ForeignPtr      (ForeignPtr, finalizeForeignPtr,
                                          mallocForeignPtrBytes)

import           Hkl.H5

--  Deal with hdf5 object in a safe way

bracket' :: MonadResource m => (a -> IO ()) -> IO a -> (a -> ConduitT i o m r) -> ConduitT i o m r
bracket' r a = bracketP (liftIO a) (liftIO . r)

withBytesC :: MonadResource m => Int -> (ForeignPtr a -> ConduitT i o m r) -> ConduitT i o m r
withBytesC n = bracket' finalizeForeignPtr (mallocForeignPtrBytes n)

withFileC :: MonadResource m => IO File -> (File -> ConduitT i o m r) -> ConduitT i o m r
withFileC = bracket' closeFile

withGroupC :: MonadResource m => IO Group -> (Group -> ConduitT i o m r) -> ConduitT i o m r
withGroupC = bracket' closeGroup

withGroupAtC :: (Location l, MonadResource m) => l -> Int -> (Group -> ConduitT i o m r) -> ConduitT i o m r
withGroupAtC l i f = do
  es <- liftIO $ nxEntries' l
  withGroupC (openGroup l (es !! i) Nothing) f

withDatasetC :: MonadResource m => IO Dataset -> (Dataset -> ConduitT i o m r) -> ConduitT i o m r
withDatasetC = bracket' closeDataset

withDataspaceC :: MonadResource m => IO Dataspace -> (Dataspace -> ConduitT i o m r) -> ConduitT i o m r
withDataspaceC = bracket' closeDataspace

withHdf5PathC :: (Location l, MonadResource m) => l -> Hdf5Path sh e -> (Dataset -> ConduitT i o m r) -> ConduitT i o m r
withHdf5PathC l (H5RootPath subpath) f = withHdf5PathC l subpath f
withHdf5PathC l (H5GroupPath n subpath) f = withGroupC (openGroup l n Nothing) $ \g -> withHdf5PathC g subpath f
withHdf5PathC l (H5GroupAtPath i subpath) f = withGroupAtC l i $ \g -> withHdf5PathC g subpath f
withHdf5PathC l (H5DatasetPath n) f = withDatasetC (openDataset l n Nothing) f
