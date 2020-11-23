{-# LANGUAGE CPP                #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hkl.DataSource ( ExtendDims(..)
                      , DataItem(..)
                      , DataSource(..)
                      , atIndex'
                      , openDataSource
                      , closeDataSource
                      ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative       ((<$>), (<*>))
#endif

import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Array.Repa           (Shape)
import           Data.ByteString.Char8     (pack)
import           Data.Vector.Storable      (Vector, any, singleton)
import           Pipes                     (lift)
import           Prelude                   hiding (any)

import           Hkl.H5

data ExtendDims = ExtendDims | StrictDims deriving (Show)

data DataItem a where
    DataItemH5 :: H5Path -> ExtendDims -> DataItem H5
    DataItemConst :: Double -> DataItem Double
deriving instance Show (DataItem a)

data DataSource a where
    DataSourceH5 :: DataItem H5 -> Dataset -> DataSource H5
    DataSourceConst :: Double -> DataSource Double

openDataSource :: File -> DataItem a -> IO (DataSource a)
openDataSource hid di@(DataItemH5 name _) = DataSourceH5 di
                                            <$> openDataset hid (pack name) Nothing
openDataSource _ (DataItemConst v) = return $ DataSourceConst v

closeDataSource :: DataSource a -> IO ()
closeDataSource (DataSourceH5 _ d)  = closeDataset d
closeDataSource (DataSourceConst _) = return ()

atIndex' :: Shape sh => DataSource a -> sh -> MaybeT IO (Vector Double)
atIndex' (DataSourceH5 _ a ) b = lift $ do
      v <- get_position_new a b
      if any isNaN v then fail "File contains Nan" else return v
atIndex' (DataSourceConst v) _ = lift $ return $ singleton v
