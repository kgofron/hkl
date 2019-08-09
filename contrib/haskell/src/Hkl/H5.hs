{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE UnicodeSyntax            #-}
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
    )
    where

import           Bindings.HDF5.Core              (HSize (HSize),
                                                  IndexType (ByName),
                                                  IterOrder (Native), hSize,
                                                  hid, indexTypeCode,
                                                  iterOrderCode)
import           Bindings.HDF5.Dataset           (Dataset, closeDataset,
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
import           Bindings.HDF5.File              (AccFlags (ReadOnly), File,
                                                  closeFile, openFile)
import           Bindings.HDF5.Raw               (H5L_info_t, HErr_t (HErr_t),
                                                  HId_t (HId_t), h5l_iterate)
import           Control.Exception               (bracket)
import           Data.Array.Repa                 (Array, Shape, listOfShape)
import           Data.Array.Repa.Repr.ForeignPtr (F, fromForeignPtr,
                                                  toForeignPtr)
import           Data.ByteString.Char8           (pack)
import           Data.IORef                      (modifyIORef', newIORef,
                                                  readIORef)
import           Data.Vector.Storable            (Vector, freeze,
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

size :: [(HSize, Maybe HSize, HSize, Maybe HSize)] -> Int
size h = product [fromIntegral c | (_, _, c, _) <- h]

get_image :: Shape sh => Detector a sh -> Dataset -> Int -> IO (Array F sh Word16)
get_image det d n = withDataspace d $ \dataspace -> do
      let s = shape det
          h = (HSize (fromIntegral n), Nothing,  HSize 1, Nothing) : shapeAsRangeToHyperslab s
      selectHyperslab dataspace Set h
      withDataspace' [HSize (fromIntegral s') | s' <- listOfShape s] $ \memspace -> do
        data_out@(MVector _ fp) <- Data.Vector.Storable.Mutable.replicate (size h) (0 :: Word16)
        readDatasetInto d (Just memspace) (Just dataspace) Nothing data_out
        return $ fromForeignPtr s fp


set_image :: Shape sh => Detector a sh -> Dataset -> Dataspace -> Int -> Array F sh Word16 -> IO ()
set_image det d dataspace n arr =  do
  selectNone dataspace
  selectHyperslab dataspace Set h
  withDataspace' [HSize (fromIntegral s') | s' <- listOfShape s] $ \memspace ->
    writeDataset d (Just memspace) (Just dataspace) Nothing vector
    where
      s = shape det
      h = (HSize (fromIntegral n), Nothing,  HSize 1, Nothing) : shapeAsRangeToHyperslab s
      fptr = toForeignPtr arr
      vector = unsafeFromForeignPtr0 fptr (size h)

getPosition' :: Dataset -> [(HSize, Maybe HSize, HSize, Maybe HSize)] -> IO (Vector Double)
getPosition' dataset h =
    withDataspace dataset $ \dataspace -> do
      selectHyperslab dataspace Set h
      withDataspace' [HSize 1] $ \memspace -> do
        data_out <- Data.Vector.Storable.Mutable.replicate 1 (0.0 :: Double)
        readDatasetInto dataset (Just memspace) (Just dataspace) Nothing data_out
        freeze data_out

get_position_new :: Shape sh => Dataset -> sh -> IO (Vector Double)
get_position_new dataset s = getPosition' dataset (shapeAsCoordinateToHyperslab s)

get_position :: Dataset -> Int -> IO (Vector Double)
get_position dataset n = getPosition' dataset [(HSize (fromIntegral n), Nothing,  HSize 1, Nothing)]

get_ub :: Dataset -> IO (Matrix Double)
get_ub dataset = do
  v <- readDataset dataset Nothing Nothing
  return $ reshape 3 v

-- | File

withH5File :: FilePath -> (File -> IO r) -> IO r
withH5File fp = bracket acquire release
    where
      acquire = openFile (pack fp) [ReadOnly] Nothing
      release = closeFile

openH5 ∷ FilePath → IO File
openH5 f = openFile (pack f) [ReadOnly] Nothing

-- | Dataspace

-- check how to merge both methods

withDataspace' :: [HSize] -> (Dataspace -> IO r) -> IO r
withDataspace' ss = bracket acquire release
  where
    acquire = createSimpleDataspace ss
    release = closeDataspace

withDataspace :: Dataset -> (Dataspace -> IO r) -> IO r
withDataspace d = bracket acquire release
  where
    acquire = getDatasetSpace d
    release = closeDataspace

lenH5Dataspace :: Dataset -> IO (Maybe Int)
lenH5Dataspace = withDataspace'' len
  where
    withDataspace'' f d = withDataspace d f
    len space_id = do
      (HSize n) <- getSimpleDataspaceExtentNPoints space_id
      return $ if n < 0 then Nothing else Just (fromIntegral n)


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
