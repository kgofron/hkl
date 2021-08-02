{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Pipes.Safe              (MonadSafe, bracket, catchAll)

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
withHdf5PathP loc (H5RootPath subpath) f = withHdf5PathP loc subpath f
withHdf5PathP loc (H5GroupPath n subpath) f = withGroupP (openGroup loc n Nothing) $ \g -> withHdf5PathP g subpath f
withHdf5PathP loc (H5GroupAtPath i subpath) f = withGroupAtP loc i $ \g -> withHdf5PathP g subpath f
withHdf5PathP loc (H5DatasetPath n) f = withDatasetP (openDataset loc n Nothing) f
withHdf5PathP loc (H5DatasetPathAttr (a, c)) f = do
  mds <- liftIO $ findDatasetAttr loc a c
  case mds of
    Nothing   -> error "can not find a dataset with the attribute "--  ++ a ++ "with content " ++ c
    (Just ds) -> withDatasetP (return ds) f
withHdf5PathP loc (H5Or l r) f = (withHdf5PathP loc l f) `catchAll` \_ -> (withHdf5PathP loc r f)
