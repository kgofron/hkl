{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-
    Copyright  : Copyright (C) 2014-2024 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Projections
  ( Space(..)
  , cmd
  , newSpace
  , saveCube
  , withMaybeLimits
  , withMaybeMask
  , withMaybeSampleAxis
  , withNPixels
  , withPixelsDims
  , withResolutions
  , withSampleAxis
   ) where

import           Control.Monad              (zipWithM)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Logger       (MonadLogger, logDebugN)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.ByteString            (useAsCString)
import           Data.Text.Encoding         (encodeUtf8)
import           Foreign.C.String           (CString, withCString)
import           Foreign.C.Types            (CBool, CSize (..))
import           Foreign.ForeignPtr         (ForeignPtr, newForeignPtr,
                                             withForeignPtr)
import           Foreign.Marshal.Alloc      (alloca)
import           Foreign.Marshal.Array      (withArrayLen)
import           Foreign.Ptr                (Ptr, nullPtr)
import           Foreign.Storable           (poke)
import           GHC.Exts                   (IsList (..))

import           Prelude                    hiding (drop)

import           Hkl.Binoculars.Config
import           Hkl.C
import           Hkl.Detector
import           Hkl.Exception
import           Hkl.Orphan                 ()
import           Hkl.Repa
import           Hkl.Utils                  hiding (withCString)


cmd :: (HasIniConfig a, MonadLogger m, MonadThrow m, MonadIO m)
    => ReaderT (Config a) m () -> Maybe FilePath -> Args a -> m ()
cmd action mf args
  = do
  content <- liftIO $ readConfig mf
  capabilities <- liftIO getCapabilities
  let econf = getConfig content args capabilities
  case econf of
    Right conf -> do
      logDebugN "config red from the config file"
      logDebugN $ serializeConfig conf
      runReaderT action conf
    Left e      -> logErrorNSH e


--  Common

withNPixels :: Detector a DIM2 -> (CSize -> IO r) -> IO r
withNPixels d f = f (toEnum . size . shape $ d)

withPixelsDims :: Array F DIM3 Double -> (Int -> Ptr CSize -> IO r) -> IO r
withPixelsDims p = withArrayLen (map toEnum $ listOfShape . extent $ p)

saveCube :: (MonadIO m, MonadThrow m, Shape sh)
         => FilePath -> String -> [Cube sh] -> m ()
saveCube o conf rs = do
  let c = mconcat rs
  case c of
    (Cube fp) -> liftIO $
      withCString o $ \fn ->
      withCString conf $ \config ->
      withForeignPtr fp $ \p -> do
      status <- c'hkl_binoculars_cube_save_hdf5 fn config p
      if status < 0
        then throwM SaveCube'Failed
        else pure ()
    EmptyCube -> throwM SaveCube'IsEmpty

newLimits :: Limits -> Double -> IO (ForeignPtr C'HklBinocularsAxisLimits)
newLimits (Limits mmin mmax) res =
    alloca $ \imin' ->
        alloca $ \imax' -> do
          imin'' <- case mmin of
                    Nothing -> pure nullPtr
                    (Just d) -> do
                              poke imin' (round (d / res))
                              pure imin'
          imax'' <- case mmax of
                    Nothing -> pure nullPtr
                    (Just d) -> do
                              poke imax' (round (d / res))
                              pure imax'
          newForeignPtr p'hkl_binoculars_axis_limits_free
                        =<< c'hkl_binoculars_axis_limits_new imin'' imax''

withMaybeLimits :: Shape sh
                => Maybe (RLimits sh)
                -> Resolutions sh
                -> (Int -> Ptr (Ptr C'HklBinocularsAxisLimits) -> IO r)
                -> IO r
withMaybeLimits mls rs f = do
  case mls of
    Nothing   -> f 0 nullPtr
    (Just ls) -> do
      ptrs <- zipWithM newLimits (toList ls) (toList rs)
      withForeignPtrs ptrs $ \pts -> withArrayLen pts f

withMaybeMask :: Maybe Mask -> (Ptr CBool -> IO r) -> IO r
withMaybeMask mm f = case mm of
                       Nothing  -> f nullPtr
                       (Just m) -> withForeignPtr (toForeignPtr m) $ \ptr -> f ptr

withMaybeSampleAxis :: Maybe SampleAxis -> (CString -> IO r) -> IO r
withMaybeSampleAxis Nothing f  = f nullPtr
withMaybeSampleAxis (Just a) f = withSampleAxis a f

withResolutions :: Shape sh => Resolutions sh -> (Int -> Ptr Double -> IO r) -> IO r
withResolutions = withArrayLen . toList

withSampleAxis :: SampleAxis -> (CString -> IO r) -> IO r
withSampleAxis (SampleAxis t) =  useAsCString (encodeUtf8 t)

-----------
-- Space --
-----------

newtype Space sh = Space (ForeignPtr C'HklBinocularsSpace)
  deriving Show

newSpace' :: Ptr C'HklBinocularsSpace -> IO (Space sh)
newSpace' p = Space <$> newForeignPtr p'hkl_binoculars_space_free p

newSpace :: (Shape sh1, Shape sh) => Detector a sh1 -> Int -> IO (Space sh)
newSpace d n = do
  let nPixels = toEnum . size . shape $ d
  let nDims = toEnum n
  newSpace' =<< c'hkl_binoculars_space_new nPixels nDims
