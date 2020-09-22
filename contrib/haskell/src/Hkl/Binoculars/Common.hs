{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Common
  ( Chunk(..)
  , InputFn(..)
  , DataFrameSpace(..)
  , LenP(..)
  , chunk
  , withCubeAccumulator
  , mkCube'P
  , mkJobs
  ) where

import           Control.Exception      (bracket)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Array.Repa        (Shape, size)
import           Data.Array.Repa.Index  (DIM2)
import           Data.IORef             (IORef, modifyIORef', newIORef,
                                         readIORef)
import           Data.Word              (Word16)
import           Foreign.ForeignPtr     (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array  (withArrayLen)
import           Foreign.Ptr            (Ptr)
import           Foreign.Storable       (peek)
import           GHC.Conc               (getNumCapabilities)
import           Path                   (Abs, File, Path, fromAbsFile)
import           Pipes                  (Consumer, Pipe, await, each, (>->))
import           Pipes.Prelude          (toListM)
import           Pipes.Safe             (SafeT, runSafeT)
import           Text.Printf            (printf)

import           Hkl.C.Binoculars
import           Hkl.Detector

data Chunk n a = Chunk !a !n !n
deriving instance (Show n, Show a) => Show (Chunk n a)

cweight :: Num n => Chunk n a -> n
cweight (Chunk _ l h) = h - l

csplit :: Num n => Chunk n a -> n -> (Chunk n a, Chunk n a)
csplit (Chunk a l h) n = (Chunk a l (l + n), Chunk a (l+n) h)

chunk :: (Num n, Ord n) => n -> [Chunk n a] -> [[Chunk n a]]
chunk target = go target target
  where
    go _ _ []          = []
    go tgt gap [x]     = golast tgt gap x
    go tgt gap ~(x:xs) =
      let gap' = gap - cweight x
      in if | gap' > 0                -> cons1 x $ go tgt gap' xs
            | gap' == 0                -> [x] : go tgt tgt xs
            | (x1, x2) <- csplit x gap -> [x1] : go tgt tgt (x2 : xs)

    cons1 x cs = (x : Prelude.head cs) : tail cs

    golast tgt gap x =
      if | cweight x <= gap         -> [[x]]
         | (x1, x2) <- csplit x gap -> [x1] : golast tgt tgt x2

{-# SPECIALIZE chunk :: Int -> [Chunk Int FilePath] -> [[Chunk Int FilePath]]  #-}

class LenP a where
  lenP :: a -> Pipe FilePath Int (SafeT IO) ()

toList :: InputFn -> [FilePath]
toList (InputFn f)           = [f]
toList (InputRange tmpl f t) = [printf tmpl i | i <- [f..t]]
toList (InputList fs)        = map fromAbsFile fs

mkJobs' :: Int -> [FilePath] -> [Int] -> [[Chunk Int FilePath]]
mkJobs' n fns ts = chunk n [Chunk f 0 t | (f, t) <- zip fns ts]

mkJobs :: LenP a => InputFn -> a -> IO [[Chunk Int FilePath]]
mkJobs fn h5d = do
  let fns = toList fn
  ns <- runSafeT $ toListM $ each fns >-> lenP h5d
  c' <- getNumCapabilities
  let ntot = sum ns
      c = if c' >= 2 then c' - 1 else c'
  return $ mkJobs' (quot ntot c) fns ns

--  DataFrameSpace

data DataFrameSpace sh = DataFrameSpace (ForeignPtr Word16) (Space sh)
  deriving Show

--  Create the Cube

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)

{-# INLINE mkCube' #-}
mkCube' :: Shape sh => Detector a DIM2 -> [DataFrameSpace sh] -> IO (Cube' sh)
mkCube' detector dfs = do
  let spaces = [spaceHklPointer s | (DataFrameSpace _ s) <- dfs]
  let images = [img | (DataFrameSpace img _) <- dfs]
  let nPixels = size . shape $ detector
  withForeignPtrs spaces $ \pspaces ->
    withForeignPtrs images $ \pimages ->
    withArrayLen pspaces $ \nSpaces' spaces' ->
    withArrayLen pimages $ \_ images' ->
    peek =<< {-# SCC "hkl_binoculars_cube_new'" #-} hkl_binoculars_cube_new' (toEnum nSpaces') spaces' (toEnum nPixels) images'

mkCube'P :: (MonadIO m, Shape sh) => Detector a DIM2 -> IORef (Cube' sh) -> Consumer (DataFrameSpace sh) m ()
mkCube'P det ref = forever $ do
  s <- await
  c2 <- liftIO $ mkCube' det [s]
  liftIO $ modifyIORef' ref (c2 <>)

type Template = String

data InputFn = InputFn FilePath
             | InputRange Template Int Int
             | InputList [Path Abs File]
  deriving Show

withCubeAccumulator :: Shape sh => (IORef (Cube' sh)  -> IO ()) -> IO (Cube' sh)
withCubeAccumulator f = bracket (newIORef EmptyCube') pure (\r -> f r >> readIORef r)
