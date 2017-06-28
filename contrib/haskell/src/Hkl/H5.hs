{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnicodeSyntax #-}
module Hkl.H5
    ( Dataset
    , File
    , H5
    , H5Path
    , check_ndims
    , closeDataset
    , closeFile
    , get_position
    , get_position_new
    , get_ub
    , lenH5Dataspace
    , nxEntries
    , openDataset
    , openH5
    , withH5File
    )
    where


import Bindings.HDF5.Core ( HSize(..)
                          , IndexType(..)
                          , IterOrder(..)
                          , hid
                          , hSize
                          , indexTypeCode
                          , iterOrderCode
                          )
import Bindings.HDF5.File ( File
                          , AccFlags(ReadOnly)
                          , openFile
                          , closeFile
                          )
import Bindings.HDF5.Group ( Group )
import Bindings.HDF5.Dataset ( Dataset
                             , openDataset
                             , closeDataset
                             , getDatasetSpace
                             , readDataset
                             , readDatasetInto
                             )
import Bindings.HDF5.Dataspace ( Dataspace
                               , SelectionOperator(Set)
                               , closeDataspace
                               , createSimpleDataspace
                               , getSimpleDataspaceExtentNDims
                               , getSimpleDataspaceExtentNPoints
                               , selectHyperslab
                               )
import Bindings.HDF5.Link ( LinkInfo, iterateLinks, visitLinks )
import Bindings.HDF5.Raw ( HErr_t(..)
                         , HId_t(..)
                         , HSize_t(..)
                         , H5L_info_t
                         , H5L_iterate_t
                         , h5l_iterate
                         )
import Control.Exception (bracket)
import Data.Array.Repa (Shape, listOfShape)
import Data.ByteString.Char8 ( pack )
import Data.IORef ( newIORef, readIORef, writeIORef)
import Data.Vector.Storable (Vector, freeze)
import Data.Vector.Storable.Mutable (replicate)
import Foreign.StablePtr ( castPtrToStablePtr
                         , castStablePtrToPtr
                         , deRefStablePtr
                         , freeStablePtr
                         , newStablePtr
                         )
import Foreign.Ptr ( FunPtr, Ptr , freeHaskellFunPtr )
import Foreign.Ptr.Conventions ( In(..)
                               , InOut(..)
                               , WrappedPtr(..)
                               , withInOut
                               , withInOut_
                               )
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types (CInt(..))
import Numeric.LinearAlgebra (Matrix, reshape)
import Prelude hiding (replicate)

{-# ANN module "HLint: ignore Use camelCase" #-}

data H5

type H5Path = String


check_ndims :: Dataset -> Int -> IO Bool
check_ndims d expected = do
  space_id <- getDatasetSpace d
  (CInt ndims) <- getSimpleDataspaceExtentNDims space_id
  return $ expected == fromEnum ndims

toHyperslab :: Shape sh => sh -> [(HSize, Maybe HSize, HSize, Maybe HSize)]
toHyperslab s = [(HSize (fromIntegral n), Just (HSize 1), HSize 1, Just (HSize 1)) | n <- listOfShape s]

get_position_new :: Shape sh => Dataset -> sh -> IO (Vector Double)
get_position_new dataset s =
    withDataspace dataset $ \dataspace -> do
      selectHyperslab dataspace Set (toHyperslab s)
      withDataspace' $ \memspace -> do
        data_out <- replicate 1 (0.0 :: Double)
        readDatasetInto dataset (Just memspace) (Just dataspace) Nothing data_out
        freeze data_out

get_position :: Dataset -> Int -> IO (Vector Double)
get_position dataset n =
    withDataspace dataset $ \dataspace -> do
      let start = HSize (fromIntegral n)
      let stride = Just (HSize 1)
      let count = HSize 1
      let block = Just (HSize 1)
      selectHyperslab dataspace Set [(start, stride, count, block)]
      withDataspace' $ \memspace -> do
        data_out <- replicate 1 (0.0 :: Double)
        readDatasetInto dataset (Just memspace) (Just dataspace) Nothing data_out
        freeze data_out

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

withDataspace' :: (Dataspace -> IO r) -> IO r
withDataspace' = bracket acquire release
  where
    acquire = createSimpleDataspace [HSize 1]
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

nxEntries ∷ FilePath → IO [String]
nxEntries f = withH5File f $ \h → do
  state <- newIORef []
  statePtr <- newStablePtr state
  let opData = InOut $ castStablePtrToPtr statePtr
  let startIndex = Nothing
  let indexType = ByName
  let order = Native
  iop <- mkOp callback
  _ <- withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
      h5l_iterate (hid h) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData

  freeHaskellFunPtr iop
  freeStablePtr statePtr

  -- retrieve the final state
  readIORef state
    where
      callback ∷ H5Iterate a
      callback _g n _i (InOut dataptr) =
          do
            let opData = castWrappedPtr dataptr
            -- get the state
            stRef <- deRefStablePtr (castPtrToStablePtr opData)
            st <- readIORef stRef

            -- compute the new state
            name <- peekCString n
            let newSt = st ++ [name]

            -- store the new state
            writeIORef stRef newSt
            return $ HErr_t 0
