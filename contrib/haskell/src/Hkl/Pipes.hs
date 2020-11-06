{-# LANGUAGE GADTs #-}
module Hkl.Pipes
    ( withBytes
    , withFileP
    , withDatasetP
    , withDataspaceP
    , withHdf5PathP
    )
    where

import           Bindings.HDF5.Core      (Location)
import           Bindings.HDF5.Dataspace (Dataspace, closeDataspace)
import           Bindings.HDF5.Group     (Group, closeGroup, openGroup)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Foreign.ForeignPtr      (ForeignPtr, mallocForeignPtrBytes,
                                          touchForeignPtr)
import           Pipes.Safe              (MonadSafe, bracket)

import           Hkl.H5

--  Deal with hdf5 object in a safe way

bracket' :: MonadSafe m => (a -> IO ()) -> IO a -> (a -> m r) -> m r
bracket' r a = bracket (liftIO a) (liftIO . r)

withBytes :: MonadSafe m => Int -> (ForeignPtr a -> m r) -> m r
withBytes n = bracket' touchForeignPtr (mallocForeignPtrBytes n)

withFileP :: MonadSafe m => IO File -> (File -> m r) -> m r
withFileP = bracket' closeFile

withGroupP :: MonadSafe m => IO Group -> (Group -> m r) -> m r
withGroupP = bracket' closeGroup

withGroupAtP :: (Location l, MonadSafe m) => l -> Int -> (Group -> m r) -> m r
withGroupAtP l i f = do
  es <- liftIO $ nxEntries' l
  withGroupP (openGroup l (es !! i) Nothing) f

withDatasetP :: MonadSafe m => IO Dataset -> (Dataset -> m r) -> m r
withDatasetP = bracket' closeDataset

withDataspaceP :: MonadSafe m => IO Dataspace -> (Dataspace -> m r) -> m r
withDataspaceP = bracket' closeDataspace

withHdf5PathP :: (MonadSafe m, Location l) => l -> Hdf5Path sh e -> (Dataset -> m r) -> m r
withHdf5PathP l (H5RootPath subpath) f = withHdf5PathP l subpath f
withHdf5PathP l (H5GroupPath n subpath) f = withGroupP (openGroup l n Nothing) $ \g -> withHdf5PathP g subpath f
withHdf5PathP l (H5GroupAtPath i subpath) f = withGroupAtP l i $ \g -> withHdf5PathP g subpath f
withHdf5PathP l (H5DatasetPath n) f = withDatasetP (openDataset l n Nothing) f
