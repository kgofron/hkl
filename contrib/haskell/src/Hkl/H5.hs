{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.H5
    ( Dataset
    , File
    , H5
    , H5Path
    , check_ndims
    , closeDataset
    , closeFile
    , get_image
    , get_position
    , get_position_new
    , get_ub
    , lenH5Dataspace
    , nxEntries
    , openDataset
    , openH5
    , set_image
    , withH5File
    -- new API
    , Hdf5
    , ToHdf5(..)
    , hdf5
    , group
    , dataset
    , saveHdf5
    )
    where

import           Bindings.HDF5.Core              (HSize (HSize),
                                                  IndexType (ByName),
                                                  IterOrder (Native), Location,
                                                  hSize, hid, indexTypeCode,
                                                  iterOrderCode)
import           Bindings.HDF5.Dataset           (Dataset, closeDataset,
                                                  createDataset,
                                                  getDatasetSpace, openDataset,
                                                  readDataset, readDatasetInto,
                                                  writeDataset)
import           Bindings.HDF5.Dataspace         (Dataspace,
                                                  SelectionOperator (Set),
                                                  closeDataspace,
                                                  createSimpleDataspace,
                                                  getSimpleDataspaceExtentNDims,
                                                  getSimpleDataspaceExtentNPoints,
                                                  selectHyperslab, selectNone)
import           Bindings.HDF5.Datatype.Internal (NativeType, nativeTypeOf)
import           Bindings.HDF5.File              (AccFlags (ReadOnly, Truncate),
                                                  File, closeFile, createFile,
                                                  openFile)
import           Bindings.HDF5.Group             (Group, closeGroup,
                                                  createGroup)
import           Bindings.HDF5.Raw               (H5L_info_t, HErr_t (HErr_t),
                                                  HId_t (HId_t), h5l_iterate)
import           Control.Exception               (bracket)
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  linearIndex, listOfShape,
                                                  size)
import           Data.Array.Repa.Repr.ForeignPtr (F, fromForeignPtr,
                                                  toForeignPtr)
import           Data.ByteString.Char8           (ByteString, pack)
import           Data.IORef                      (modifyIORef', newIORef,
                                                  readIORef)
import           Data.Vector.Storable            (Storable, Vector, freeze,
                                                  unsafeFromForeignPtr0)
import           Data.Vector.Storable.Mutable    (MVector (..), replicate)
import           Data.Word                       (Word16)
import           Foreign.C.String                (CString, peekCString)
import           Foreign.C.Types                 (CInt (CInt))
import           Foreign.Ptr                     (FunPtr, freeHaskellFunPtr)
import           Foreign.Ptr.Conventions         (In (In), InOut (InOut),
                                                  castWrappedPtr, withInOut_)
import           Foreign.StablePtr               (StablePtr, castPtrToStablePtr,
                                                  castStablePtrToPtr,
                                                  deRefStablePtr, freeStablePtr,
                                                  newStablePtr)
import           Numeric.LinearAlgebra           (Matrix, reshape)

import           Hkl.Detector

{-# ANN module "HLint: ignore Use camelCase" #-}


data H5

type H5Path = String

check_ndims :: Dataset -> Int -> IO Bool
check_ndims d expected = do
  space_id <- getDatasetSpace d
  (CInt ndims) <- getSimpleDataspaceExtentNDims space_id
  return $ expected == fromEnum ndims

shapeAsCoordinateToHyperslab :: Shape sh => sh -> [(HSize, Maybe HSize, HSize, Maybe HSize)]
shapeAsCoordinateToHyperslab s = [(HSize (fromIntegral n), Nothing, HSize 1, Nothing) | n <- listOfShape s]

shapeAsRangeToHyperslab :: Shape sh => sh -> [(HSize, Maybe HSize, HSize, Maybe HSize)]
shapeAsRangeToHyperslab s =  [(0, Nothing, HSize (fromIntegral s'), Nothing) | s' <- listOfShape s]

createDataspaceFromShape :: Shape sh => sh -> IO Dataspace
createDataspaceFromShape sh = createSimpleDataspace [HSize (fromIntegral s) | s <- listOfShape sh]

castToVector :: (Shape sh, Storable e) => Array F sh e -> Vector e
castToVector arr = unsafeFromForeignPtr0 (toForeignPtr arr) (size . extent $ arr)

get_image :: Shape sh => Detector a sh -> Dataset -> Int -> IO (Array F sh Word16)
get_image det d n = withDataspace (getDatasetSpace d) $ \dataspace -> do
      let s = shape det
          h = (HSize (fromIntegral n), Nothing,  HSize 1, Nothing) : shapeAsRangeToHyperslab s
      selectHyperslab dataspace Set h
      withDataspace (createDataspaceFromShape s) $ \memspace -> do
        data_out@(MVector _ fp) <- Data.Vector.Storable.Mutable.replicate (size s) (0 :: Word16)
        readDatasetInto d (Just memspace) (Just dataspace) Nothing data_out
        return $ fromForeignPtr s fp

set_image :: Shape sh => Detector a sh -> Dataset -> Dataspace -> Int -> Array F sh Word16 -> IO ()
set_image det d dataspace n arr =  do
  selectNone dataspace
  selectHyperslab dataspace Set h
  withDataspace (createDataspaceFromShape s) $ \memspace ->
    writeDataset d (Just memspace) (Just dataspace) Nothing (castToVector arr)
    where
      s = shape det
      h = (HSize (fromIntegral n), Nothing,  HSize 1, Nothing) : shapeAsRangeToHyperslab s

saveRepa :: (Location t, NativeType e, Shape sh, Storable e) => t -> ByteString -> Array F sh e -> IO ()
saveRepa f path arr =
  withDataspace (createDataspaceFromShape (extent arr)) $ \dataspace ->
  withDataset (createDataset f path (nativeTypeOf (linearIndex arr 0)) dataspace Nothing Nothing Nothing) $ \dataset' ->
  writeDataset dataset' Nothing Nothing Nothing (castToVector arr)

getPosition' :: Dataset -> [(HSize, Maybe HSize, HSize, Maybe HSize)] -> IO (Vector Double)
getPosition' dataset' h =
    withDataspace (getDatasetSpace dataset') $ \dataspace -> do
      selectHyperslab dataspace Set h
      withDataspace (createSimpleDataspace [HSize 1]) $ \memspace -> do
        data_out <- Data.Vector.Storable.Mutable.replicate 1 (0.0 :: Double)
        readDatasetInto dataset' (Just memspace) (Just dataspace) Nothing data_out
        freeze data_out

get_position_new :: Shape sh => Dataset -> sh -> IO (Vector Double)
get_position_new dataset' s = getPosition' dataset' (shapeAsCoordinateToHyperslab s)

get_position :: Dataset -> Int -> IO (Vector Double)
get_position dataset' n = getPosition' dataset' [(HSize (fromIntegral n), Nothing,  HSize 1, Nothing)]

get_ub :: Dataset -> IO (Matrix Double)
get_ub dataset' = do
  v <- readDataset dataset' Nothing Nothing
  return $ reshape 3 v

-- | File

withH5File' :: IO File -> (File -> IO r) -> IO r
withH5File' a = bracket a closeFile

withH5File :: FilePath -> (File -> IO r) -> IO r
withH5File fp = withH5File' (openFile (pack fp) [ReadOnly] Nothing)

openH5 ∷ FilePath → IO File
openH5 f = openFile (pack f) [ReadOnly] Nothing

-- | Group

withGroup :: IO Group -> (Group -> IO r) -> IO r
withGroup a = bracket a closeGroup

-- | Dataspace

-- check how to merge both methods

withDataspace :: IO Dataspace -> (Dataspace -> IO r) -> IO r
withDataspace a = bracket a closeDataspace

lenH5Dataspace :: Dataset -> IO (Maybe Int)
lenH5Dataspace d = withDataspace (getDatasetSpace d) len
  where
    len space_id = do
      (HSize n) <- getSimpleDataspaceExtentNPoints space_id
      return $ if n < 0 then Nothing else Just (fromIntegral n)


-- | DataSet

withDataset :: IO Dataset -> (Dataset -> IO r) -> IO r
withDataset a = bracket a closeDataset

-- | WIP until I have decided what is the right way to go

type H5Iterate a = HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t

foreign import ccall "wrapper" mkOp :: H5Iterate a -> IO (FunPtr (H5Iterate a))

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr

withFunPtr :: H5Iterate a -> (FunPtr (H5Iterate a) -> IO b) -> IO b
withFunPtr f = bracket (mkOp f) freeHaskellFunPtr

nxEntries ∷ FilePath → IO [String]
nxEntries f = withH5File f $ \h → do
  state <- newIORef []
  _ <- withStablePtr state $ \statePtr -> do
    let opData = InOut $ castStablePtrToPtr statePtr
    let startIndex = Nothing
    let indexType = ByName
    let order = Native
    withFunPtr callback $ \iop ->
      withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
      h5l_iterate (hid h) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData

  -- retrieve the final state
  readIORef state
    where
      callback ∷ H5Iterate a
      callback _g n _i (InOut dataptr) = do
        name <- peekCString n
        stRef <- deRefStablePtr (castPtrToStablePtr . castWrappedPtr $ dataptr)
        modifyIORef' stRef $ \st -> name : st
        return $ HErr_t 0


-- | Better API

data Hdf5M a
  = H5Root (Hdf5M a)
  | H5Group ByteString [Hdf5M a]
  | forall sh b. (NativeType b, Shape sh) => H5Dataset ByteString (Array F sh b)

type Hdf5 = Hdf5M ()

class ToHdf5 a where
  toHdf5 :: a -> Hdf5

hdf5 :: Hdf5 -> Hdf5
hdf5 = H5Root

group :: ByteString -> [Hdf5] -> Hdf5
group g = H5Group g

dataset :: (NativeType b, Shape sh) => ByteString -> (Array F sh b) -> Hdf5
dataset = H5Dataset

saveHdf5 :: ToHdf5 a => FilePath -> a -> IO ()
saveHdf5 f a =  withH5File' (createFile (pack f) [Truncate] Nothing Nothing) $ \f' ->
  go f' (toHdf5 a)
  where
    go :: Location l => l -> Hdf5 -> IO ()
    go l (H5Root c) = go l c
    go l (H5Group n cs) = withGroup (createGroup l n Nothing Nothing Nothing ) $ \g -> mapM_ (go g) cs
    go l (H5Dataset n arr) = saveRepa l n arr
