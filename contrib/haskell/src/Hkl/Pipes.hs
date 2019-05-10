{-# LANGUAGE GADTs #-}
module Hkl.Pipes
    ( withFileP
    , withDatasetP
    , withDataspaceP
    )
    where

import Bindings.HDF5.File ( File, closeFile )
import Bindings.HDF5.Dataset ( Dataset, closeDataset )
import Bindings.HDF5.Dataspace ( Dataspace, closeDataspace )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Pipes.Safe (MonadSafe, bracket)

-- | Deal with hdf5 object in a safe way

bracket' :: MonadSafe m => (a -> IO ()) -> IO a -> (a -> m r) -> m r
bracket' r a = bracket (liftIO a) (liftIO . r)

withFileP :: MonadSafe m => IO File -> (File -> m r) -> m r
withFileP = bracket' closeFile

withDatasetP :: MonadSafe m => IO Dataset -> (Dataset -> m r) -> m r
withDatasetP = bracket' closeDataset

withDataspaceP :: MonadSafe m => IO Dataspace -> (Dataspace -> m r) -> m r
withDataspaceP = bracket' closeDataspace
