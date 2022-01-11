{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Common
  ( Chunk(..)
  , DataFrameSpace(..)
  , InputFn(..)
  , addSpace
  , chunk
  , clength
  , mkCube'
  , toList
  , withCubeAccumulator
  ) where

import           Control.Exception     (bracket)
import           Data.Array.Repa       (Shape)
import           Data.IORef            (IORef, newIORef, readIORef)
import           Foreign.ForeignPtr    (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array (withArrayLen)
import           Foreign.Storable      (peek)
import           Path                  (Abs, File, Path, fromAbsFile)
import           Text.Printf           (printf)

import           Hkl.C.Binoculars
import           Hkl.Image
import           Hkl.Orphan            ()

data Chunk n a = Chunk !a !n !n
deriving instance (Show n, Show a) => Show (Chunk n a)

clength :: Num n => Chunk n a -> n
clength (Chunk _ l h) = h - l + 1
{-# SPECIALIZE clength :: Chunk Int FilePath -> Int  #-}

cweight :: Num n => Chunk n a -> n
cweight (Chunk _ l h) = h - l

csplit :: Num n => Chunk n a -> n -> (Chunk n a, Chunk n a)
csplit (Chunk a l h) n = (Chunk a l (l + n), Chunk a (l+n) h)

chunk :: (Num n, Ord n) => n -> [Chunk n a] -> [[Chunk n a]]
chunk target = go target target
  where
    go :: (Num n, Ord n) => n -> n -> [Chunk n a] -> [[Chunk n a]]
    go _ _ []          = []
    go tgt gap [x]     = golast tgt gap x
    go tgt gap ~(x:xs) =
      let gap' = gap - cweight x
      in if | gap' > 0                 -> cons1 x $ go tgt gap' xs
            | gap' == 0                -> [x] : go tgt tgt xs
            | (x1, x2) <- csplit x gap -> [x1] : go tgt tgt (x2 : xs)

    cons1 x cs = (x : Prelude.head cs) : tail cs

    golast tgt gap x =
      if | cweight x <= gap         -> [[x]]
         | (x1, x2) <- csplit x gap -> [x1] : golast tgt tgt x2

{-# SPECIALIZE chunk :: Int -> [Chunk Int FilePath] -> [[Chunk Int FilePath]]  #-}

toList :: InputFn -> [FilePath]
toList (InputFn f)           = [f]
toList (InputRange tmpl f t) = [printf tmpl i | i <- [f..t]]
toList (InputList fs)        = map fromAbsFile fs

--  DataFrameSpace

data DataFrameSpace sh = DataFrameSpace Image (Space sh) Double
  deriving Show

--  Create the Cube

{-# INLINE mkCube' #-}
mkCube' :: Shape sh => [DataFrameSpace sh] -> IO (Cube' sh)
mkCube' dfs = do
  let spaces = [spaceHklPointer s | (DataFrameSpace _ s _) <- dfs]
  withForeignPtrs spaces $ \pspaces ->
    withArrayLen pspaces $ \nSpaces' spaces' ->
    peek =<< {-# SCC "hkl_binoculars_cube_new'" #-} hkl_binoculars_cube_new' (toEnum nSpaces') spaces'

{-# INLINE addSpace #-}
addSpace :: Shape sh => DataFrameSpace sh -> Cube' sh -> IO (ForeignPtr (Cube' sh))
addSpace df EmptyCube' = do
  (Cube' fp) <- mkCube' [df]
  return fp
addSpace (DataFrameSpace _ s _) (Cube' fp) =
    withForeignPtr (spaceHklPointer s) $ \spacePtr ->
    withForeignPtr fp $ \cPtr -> do
      {-# SCC "hkl_binoculars_cube_add_space" #-} hkl_binoculars_cube_add_space cPtr spacePtr
      return fp

type Template = String

data InputFn = InputFn FilePath
             | InputRange Template Int Int
             | InputList [Path Abs File]
  deriving Show

withCubeAccumulator :: Shape sh => Cube' sh -> (IORef (Cube' sh)  -> IO ()) -> IO (Cube' sh)
withCubeAccumulator c f = bracket
  (newIORef =<< peek =<< case c of
                           EmptyCube' -> hkl_binoculars_cube_new_empty'
                           (Cube' fp) -> withForeignPtr fp $ \p -> hkl_binoculars_cube_new_empty_from_cube' p
  )
  pure
  (\r -> f r >> readIORef r)

-- Projections
