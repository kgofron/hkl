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
    , get_image'
    , get_position
    , get_position_new
    , get_ub
    , lenH5Dataspace
    , nxEntries
    , nxEntries'
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
    , Hdf5Path(..)
    , hdf5p
    , groupp
    , grouppat
    , datasetp
    , withHdf5Path
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
import           Bindings.HDF5.Datatype.Internal (NativeType, hdfTypeOf1,
                                                  nativeTypeOf)
import           Bindings.HDF5.Error             (withErrorCheck_)
import           Bindings.HDF5.File              (AccFlags (ReadOnly, Truncate),
                                                  File, closeFile, createFile,
                                                  openFile)
import           Bindings.HDF5.Group             (Group, closeGroup,
                                                  createGroup, openGroup)
import           Bindings.HDF5.PropertyList.DXPL (DXPL)
import           Bindings.HDF5.Raw               (H5L_info_t, HErr_t (HErr_t),
                                                  HId_t (HId_t), h5d_read,
                                                  h5l_iterate, h5p_DEFAULT,
                                                  h5s_ALL)
import           Control.Exception               (bracket)
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  linearIndex, listOfShape,
                                                  size)
import           Data.Array.Repa.Repr.ForeignPtr (F, fromForeignPtr,
                                                  toForeignPtr)
import           Data.ByteString.Char8           (ByteString, pack, packCString,
                                                  unpack)
import           Data.IORef                      (modifyIORef', newIORef,
                                                  readIORef)
import           Data.Vector.Storable            (Storable, Vector, freeze,
                                                  unsafeFromForeignPtr0)
import           Data.Vector.Storable.Mutable    (MVector (..), replicate)
import           Data.Word                       (Word16)
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CInt (CInt))
import           Foreign.ForeignPtr              (ForeignPtr, newForeignPtr)
import           Foreign.Marshal.Alloc           (finalizerFree, mallocBytes)
import           Foreign.Ptr                     (FunPtr, Ptr,
                                                  freeHaskellFunPtr)
import           Foreign.Ptr.Conventions         (In (In), InOut (InOut),
                                                  OutArray (..), castWrappedPtr,
                                                  withInOut_)
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
shapeAsCoordinateToHyperslab s = [(HSize (fromIntegral n), Nothing, HSize 1, Nothing) | n <- reverse . listOfShape $ s]

shapeAsRangeToHyperslab :: Shape sh => sh -> [(HSize, Maybe HSize, HSize, Maybe HSize)]
shapeAsRangeToHyperslab s =  [(0, Nothing, HSize (fromIntegral s'), Nothing) | s' <- reverse . listOfShape $ s]

createDataspaceFromShape :: Shape sh => sh -> IO Dataspace
createDataspaceFromShape sh = createSimpleDataspace [HSize (fromIntegral s) | s <- reverse . listOfShape $ sh]

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

get_image' :: Shape sh => Detector a sh -> Dataset -> Int -> IO (ForeignPtr Word16)
get_image' det d n = withDataspace (getDatasetSpace d) $ \dataspace -> do
      let s = shape det
          h = (HSize (fromIntegral n), Nothing,  HSize 1, Nothing) : shapeAsRangeToHyperslab s
      selectHyperslab dataspace Set h
      withDataspace (createDataspaceFromShape s) $ \memspace -> do
        p <- mallocBytes ((size s) * 2)
        readDatasetInto' d (Just memspace) (Just dataspace) Nothing p
        fp <- newForeignPtr finalizerFree p
        return fp

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

withGroupAt :: Location l => l -> Int -> (Group -> IO r) -> IO r
withGroupAt l i f = do
  es <- nxEntries' l
  withGroup (openGroup l (es !! i) Nothing) f

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

readDatasetInto' :: NativeType t =>
                   Dataset ->
                   Maybe Dataspace ->
                   Maybe Dataspace ->
                   Maybe DXPL ->
                   Ptr t ->
                   IO ()
readDatasetInto' dset mem_space_id file_space_id plist_id buf = do
    withErrorCheck_ $ h5d_read (hid dset) (hdfTypeOf1 buf) (maybe h5s_ALL hid mem_space_id) (maybe h5s_ALL hid file_space_id) (maybe h5p_DEFAULT hid plist_id) (OutArray buf)

-- | WIP until I have decided what is the right way to go

type H5Iterate a = HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t

foreign import ccall "wrapper" mkOp :: H5Iterate a -> IO (FunPtr (H5Iterate a))

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr

withFunPtr :: H5Iterate a -> (FunPtr (H5Iterate a) -> IO b) -> IO b
withFunPtr f = bracket (mkOp f) freeHaskellFunPtr

nxEntries ∷ FilePath → IO [String]
nxEntries f = withH5File f $ \l → do
  es <- nxEntries' l
  return $ map unpack es

nxEntries' :: Location l => l -> IO [ByteString]
nxEntries' l = do
  state <- newIORef []
  _ <- withStablePtr state $ \statePtr -> do
    let opData = InOut $ castStablePtrToPtr statePtr
    let startIndex = Nothing
    let indexType = ByName
    let order = Native
    withFunPtr callback $ \iop ->
      withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
      h5l_iterate (hid l) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData

  -- retrieve the final state
  readIORef state
    where
      callback :: H5Iterate a
      callback _ n _ (InOut dataptr) = do
        name <- packCString n
        stRef <- deRefStablePtr. castPtrToStablePtr . castWrappedPtr $ dataptr
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


data Hdf5Path sh e
  = H5RootPath (Hdf5Path sh e)
  | H5GroupPath ByteString (Hdf5Path sh e)
  | H5GroupAtPath Int (Hdf5Path sh e)
  | H5DatasetPath ByteString

hdf5p :: Hdf5Path sh e -> Hdf5Path sh e
hdf5p = H5RootPath

groupp :: ByteString -> Hdf5Path sh e -> Hdf5Path sh e
groupp = H5GroupPath

grouppat :: Int -> Hdf5Path sh e -> Hdf5Path sh e
grouppat = H5GroupAtPath

datasetp :: ByteString -> Hdf5Path sh e
datasetp = H5DatasetPath

withHdf5Path :: FilePath -> Hdf5Path sh e -> (Dataset -> IO r) -> IO r
withHdf5Path fn path f = withH5File fn $ \fn' -> go fn' path f
  where
    go :: Location l => l -> Hdf5Path sh e -> (Dataset -> IO r) -> IO r
    go l (H5RootPath subpath) f' = go l subpath f'
    go l (H5GroupPath n subpath) f' = withGroup (openGroup l n Nothing) $ \g -> go g subpath f'
    go l (H5GroupAtPath i subpath) f' = withGroupAt l i $ \g -> go g subpath f'
    go l (H5DatasetPath n) f' = withDataset (openDataset l n Nothing) f'
