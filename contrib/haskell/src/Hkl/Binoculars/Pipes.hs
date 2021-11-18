{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

{-
    Copyright  : Copyright (C) 2014-2021 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Pipes
  ( Chunk(..)
  , LenP(..)
  , processHklP
  , processQxQyQzP
  ) where

import           Bindings.HDF5.Core                (Location)
import           Bindings.HDF5.Dataset             (getDatasetType)
import           Bindings.HDF5.Datatype            (getTypeSize, nativeTypeOf,
                                                    typeIDsEqual)
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Exception                 (throwIO)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.Catch               (MonadThrow, tryJust)
import           Control.Monad.Extra               (ifM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger, logInfo)
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Array.Repa                   (Shape, size)
import           Data.Array.Repa.Index             (DIM1, DIM2)
import           Data.IORef                        (IORef, readIORef)
import           Data.Int                          (Int32)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (pack)
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16, Word32)
import           GHC.Base                          (returnIO)
import           GHC.Conc                          (getNumCapabilities)
import           GHC.Float                         (float2Double)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Quantity, Unit, degree,
                                                    (*~))
import           Pipes                             (Consumer, Pipe, Proxy,
                                                    await, each, runEffect,
                                                    yield, (>->))
import           Pipes.Prelude                     (tee, toListM)
import           Pipes.Safe                        (MonadSafe, SafeT,
                                                    SomeException, bracket,
                                                    catchP, displayException,
                                                    runSafeP, runSafeT)
import           System.ProgressBar                (Progress (..), ProgressBar,
                                                    Style (..), defStyle,
                                                    elapsedTime, incProgress,
                                                    newProgressBar,
                                                    renderDuration,
                                                    updateProgress)
import           Text.Printf                       (printf)

import           Prelude                           hiding (filter)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Types


-- LenP

class LenP a where
  lenP :: a -> Pipe FilePath Int (SafeT IO) ()


-- Jobs

mkJobs :: LenP a => InputFn -> a -> IO ([[Chunk Int FilePath]], ProgressBar ())
mkJobs fn h5d = do
  let fns = concatMap (replicate 1) (toList fn)
  ns <- runSafeT $ toListM $ each fns >-> lenP h5d
  c' <- getNumCapabilities
  let ntot = sum ns
      c = if c' >= 2 then c' - 1 else c'
  pb <- newProgressBar defStyle{ stylePostfix=elapsedTime renderDuration }
                10 (Progress 0 ntot ())
  return (mkJobs' (quot ntot c) fns ns, pb)


-- Project

withSpace :: (MonadSafe m, Shape sh)
            => Detector a DIM2 -> Int -> (Space sh -> m r) -> m r
withSpace d n = bracket (liftIO $ newSpace d n) pure

project :: (MonadSafe m, Shape sh)
          => Detector a DIM2
        -> Int
        -> (Space sh -> b -> IO (DataFrameSpace sh))
        -> Pipe b (DataFrameSpace sh) m ()
project d n f = withSpace d n $ \s -> forever $ do
               df <- await
               yield =<< liftIO (f s df)

skipMalformed :: MonadSafe m =>
                Proxy a' a b' b m r
              -> Proxy a' a b' b m r
skipMalformed p = loop
  where
    loop = catchP p $ \e -> do
        liftIO $ Prelude.print $ displayException (e :: SomeException)
        loop

-- QxQyQz

class LenP a => FramesQxQyQzP a where
  framesQxQyQzP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) DataFrameQxQyQz (SafeT IO) ()

class (FramesQxQyQzP a, Show a) => ProcessQxQyQzP a where
  processQxQyQzP :: (MonadIO m, MonadLogger m, MonadThrow m)
                 => BinocularsConfig -> (BinocularsConfig -> m a)-> m ()
  processQxQyQzP conf mkPaths = do
    let det = fromMaybe defaultDetector (_binocularsInputDetector conf)
    let output' = case _binocularsInputInputRange conf of
                   Just r  -> destination' r (_binocularsDispatcherDestination conf)
                   Nothing -> destination' (ConfigRange []) (_binocularsDispatcherDestination conf)
    let centralPixel' = _binocularsInputCentralpixel conf
    let sampleDetectorDistance = _binocularsInputSdd conf
    let detrot = fromMaybe (0 *~ degree) ( _binocularsInputDetrot conf)

    h5d <- mkPaths conf
    filenames <- InputList <$> files conf
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot
    res <- getResolution conf 3
    mask' <- getMask conf det

    let fns = concatMap (replicate 1) (toList filenames)
    ns <- liftIO $ runSafeT $ toListM $ each fns >-> lenP h5d
    cap' <-  liftIO $ getNumCapabilities
    let ntot = sum ns
    let cap = if cap' >= 2 then cap' - 1 else cap'
    let jobs = mkJobs' (quot ntot cap) fns ns

    $(logInfo) (pack $ printf "let's do a QxQyQz projection of %d image(s) on %d core(s)" ntot cap)

    pb <- liftIO $ newProgressBar defStyle{ stylePostfix=elapsedTime renderDuration } 10 (Progress 0 ntot ())

    r' <- liftIO $ mapConcurrently (\job -> withCubeAccumulator $ \c ->
                                      runSafeT $ runEffect $
                                      each job
                                      >-> framesQxQyQzP h5d det
                                      -- >-> filter (\(DataFrameQxQyQz _ _ _ ma) -> isJust ma)
                                      >-> project det 3 (spaceQxQyQz det pixels res mask')
                                      >-> tee (accumulateP c)
                                      >-> progress pb
                                  ) jobs
    liftIO $ saveCube output' r'

    liftIO $ updateProgress pb $ \p@(Progress _ t _) -> p{progressDone=t}

instance ProcessQxQyQzP QxQyQzPath

-- Hkl

class LenP a => FramesHklP a where
  framesHklP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) (DataFrameHkl b) (SafeT IO) ()


class (FramesHklP a, Show a) => ProcessHklP a where
  processHklP :: (MonadIO m, MonadLogger m, MonadThrow m)
              => BinocularsConfig -> (BinocularsConfig -> m a) -> m ()
  processHklP conf mkPaths = do
    let det = fromMaybe defaultDetector (_binocularsInputDetector conf)
    let output' = case _binocularsInputInputRange conf of
                   Just r  -> destination' r (_binocularsDispatcherDestination conf)
                   Nothing -> destination' (ConfigRange []) (_binocularsDispatcherDestination conf)
    let centralPixel' = _binocularsInputCentralpixel conf
    let sampleDetectorDistance = _binocularsInputSdd conf
    let detrot = fromMaybe (0 *~ degree) ( _binocularsInputDetrot conf)

    filenames <- InputList <$> files conf
    mask' <- getMask conf det
    res <- getResolution conf 3
    h5d <- mkPaths conf
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

    let fns = concatMap (replicate 1) (toList filenames)
    ns <- liftIO $ runSafeT $ toListM $ each fns >-> lenP h5d
    cap' <-  liftIO $ getNumCapabilities
    let ntot = sum ns
    let cap = if cap' >= 2 then cap' - 1 else cap'
    let jobs = mkJobs' (quot ntot cap) fns ns

    $(logInfo) (pack $ printf "let's do an Hkl projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    pb <- liftIO $ newProgressBar defStyle{ stylePostfix=elapsedTime renderDuration } 10 (Progress 0 ntot ())

    r' <- liftIO $ mapConcurrently (\job -> withCubeAccumulator $ \c ->
                                      runEffect $ runSafeP $
                                      each job
                                      -- >-> tee Pipes.Prelude.print
                                      >-> framesHklP h5d det
                                      -- >-> filter (\(DataFrameHkl (DataFrameQxQyQz _ _ _ ma) _) -> isJust ma)
                                      >-> project det 3 (spaceHkl conf det pixels res mask')
                                      >-> tee (accumulateP c)
                                      >-> progress pb
                                  ) jobs
    liftIO $ saveCube output' r'

    liftIO $ updateProgress pb $ \p@(Progress _ t _) -> p{progressDone=t}

instance ProcessHklP HklPath

--  Create the Cube

accumulateP :: (MonadIO m, Shape sh) => IORef (Cube' sh) -> Consumer (DataFrameSpace sh) m ()
accumulateP ref =
    forever $ do s <- await
                 liftIO $ addSpace s =<< readIORef ref

progress :: MonadIO m => ProgressBar s -> Consumer a m ()
progress p = forever $ do
  _ <- await
  liftIO $ p `incProgress` 1

-- Instances

withDetectorPathP :: (MonadSafe m, Location l) => l -> Detector a DIM2 -> DetectorPath -> ((Int -> IO Image) -> m r) -> m r
withDetectorPathP f det (DetectorPath p) g = do
  withHdf5PathP f p $ \p' -> do
    t <- liftIO $ getDatasetType p'
    s <- liftIO $ getTypeSize t
    let n = (size . shape $ det) * fromEnum s
    (ifM (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word16)))
      (withBytes n $ \buf -> g (\i -> ImageWord16 <$> getArrayInBuffer buf det p' i))
      (ifM (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Int32)))
        (withBytes n $ \buf -> g (\i -> ImageInt32 <$> getArrayInBuffer buf det p' i))
        (ifM (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word32)))
         (withBytes n $ \buf -> g (\i -> ImageWord32 <$> getArrayInBuffer buf det p' i))
         undefined)))

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

withAxesPathP :: (MonadSafe m, Location l) => l -> [Hdf5Path DIM1 Double] -> ([Dataset] -> m a) -> m a
withAxesPathP f dpaths = nest (map (withHdf5PathP f) dpaths)

withGeometryPathP :: (MonadSafe m, Location l) => l -> GeometryPath -> ((Int -> IO Geometry) -> m r) -> m r
withGeometryPathP f (GeometryPathCristalK6C w m ko ka kp g d) gg =
    withHdf5PathP f w $ \w' ->
    withHdf5PathP f m $ \mu' ->
    withHdf5PathP f ko $ \komega' ->
    withHdf5PathP f ka $ \kappa' ->
    withHdf5PathP f kp $ \kphi' ->
    withHdf5PathP f g $ \gamma' ->
    withHdf5PathP f d $ \delta' -> do
      wavelength <- liftIO $ getValueWithUnit w' 0 angstrom
      mu <- liftIO $ get_position mu' 0
      komega <- liftIO $ get_position komega' 0
      kappa <- liftIO $ get_position kappa' 0
      gamma <- liftIO $ get_position gamma' 0
      delta <- liftIO $ get_position delta' 0
      gg (\j -> do
            kphi <- get_position kphi' j
            return (Geometry
                    K6c
                    (Source wavelength)
                    (fromList [mu, komega, kappa, kphi, gamma, delta])
                    Nothing))
withGeometryPathP f (GeometryPathFix w) gg =
  withHdf5PathP f w $ \w' ->
                        gg (const $
                             Geometry Fixe
                             <$> (Source <$> getValueWithUnit w' 0 angstrom)
                             <*> pure (fromList [])
                             <*> pure Nothing)
withGeometryPathP f (GeometryPathMars w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry Mars
                 <$> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> do
                         vs <- Prelude.mapM (`get_position` j) as'
                         return (0.0 : vs))
                 <*> pure Nothing)
withGeometryPathP f (GeometryPathMedH w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry MedH
                 <$> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> do
                         vs <- Prelude.mapM (`get_position` j) as'
                         return (0.0 : vs))
                 <*> pure Nothing)
withGeometryPathP f (GeometryPathMedV w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry MedV
                 <$> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> do
                         vs <- Prelude.mapM (`get_position` j) as'
                         return (0.0 : vs))
                 <*> pure Nothing)
withGeometryPathP _f (GeometryPathMedVEiger _w _as _eix _eiz) _gg = undefined
withGeometryPathP f (GeometryPathUhv w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry Uhv
                 <$> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> Prelude.mapM (`get_position` j) as')
                 <*> pure Nothing)

withAttenuationPathP :: (MonadSafe m, Location l) =>
                       l
                     -> AttenuationPath
                     -> ((Int -> IO Double) -> m r)
                     -> m r
withAttenuationPathP f matt g =
    case matt of
      NoAttenuation -> g (const $ returnIO 1)
      (AttenuationPath p offset coef) ->
          withHdf5PathP f p $ \p' -> g (\j -> do
                                          v <-  get_position p' (j + offset)
                                          if v == badAttenuation
                                          then throwIO (WrongAttenuation "file" (j + offset) (float2Double v))
                                          else return  (coef ** float2Double v))

withQxQyQzPath :: (MonadSafe m, Location l) =>
                 l
               -> Detector a DIM2
               -> QxQyQzPath
               -> ((Int -> IO DataFrameQxQyQz) -> m r)
               -> m r
withQxQyQzPath f det (QxQyQzPath att d dif) g =
  withAttenuationPathP f att $ \getAttenuation ->
  withDetectorPathP f det d $ \getImage ->
  withGeometryPathP f dif $ \getDiffractometer ->
  g (\j -> DataFrameQxQyQz j
          <$> getAttenuation j
          <*> getDiffractometer j
          <*> getImage j
    )

--  FramesQxQyQzP

instance LenP QxQyQzPath where
    lenP (QxQyQzPath ma (DetectorPath i) _) =
        skipMalformed $ forever $ do
                           fp <- await
                           withFileP (openH5 fp) $ \f ->
                               withHdf5PathP f i $ \i' -> do
                                    (_, ss) <- liftIO $ datasetShape i'
                                    case head ss of
                                      (Just n) -> yield $ fromIntegral n - case ma of
                                                                            NoAttenuation           -> 0
                                                                            (AttenuationPath _ off _) -> off
                                      Nothing  -> error "can not extract length"

tryYield :: IO r -> Proxy x' x () r (SafeT IO) ()
tryYield io = do
  edf <- liftIO $ tryJust selectHklBinocularsException io
  case edf of
    Left _   -> return ()
    Right df -> yield df
  where
    selectHklBinocularsException :: HklBinocularsException -> Maybe HklBinocularsException
    selectHklBinocularsException e = Just e

instance FramesQxQyQzP QxQyQzPath where
    framesQxQyQzP p det =
        skipMalformed $ forever $ do
          (Chunk fp from to) <- await
          withFileP (openH5 fp) $ \f ->
            withQxQyQzPath f det p $ \getDataFrameQxQyQz ->
            forM_ [from..to-1] (tryYield . getDataFrameQxQyQz)

-- FramesHklP

getValueWithUnit :: Dataset -> Int -> Unit m d Double -> IO (Quantity d Double)
getValueWithUnit d j u = do
  v <- get_position d j
  return $ v *~ u

withSamplePathP :: (MonadSafe m, Location l) => l -> SamplePath -> (IO (Sample Triclinic) -> m r) -> m r
withSamplePathP f (SamplePath a b c alpha beta gamma ux uy uz) g =
    withHdf5PathP f a $ \a' ->
    withHdf5PathP f b $ \b' ->
    withHdf5PathP f c $ \c' ->
    withHdf5PathP f alpha $ \alpha' ->
    withHdf5PathP f beta $ \beta' ->
    withHdf5PathP f gamma $ \gamma' ->
    withHdf5PathP f ux $ \ux' ->
    withHdf5PathP f uy $ \uy' ->
    withHdf5PathP f uz $ \uz' ->
        g (Sample "test"
           <$> (Triclinic
                <$> getValueWithUnit a' 0 angstrom
                <*> getValueWithUnit b' 0 angstrom
                <*> getValueWithUnit c' 0 angstrom
                <*> getValueWithUnit alpha' 0 degree
                <*> getValueWithUnit beta' 0 degree
                <*> getValueWithUnit gamma' 0 degree)
           <*> (Parameter "ux"
                <$> get_position ux' 0
                <*> pure (Range 0 0))
           <*> (Parameter "uy"
                <$> get_position uy' 0
                <*> pure (Range 0 0))
           <*> (Parameter "uz"
                <$> get_position uz' 0
                <*> pure (Range 0 0)))
withSamplePathP _ (SamplePath2 s) g = g (return s)

instance LenP HklPath where
  lenP (HklPath p _)           = lenP p

instance FramesHklP HklPath where
  framesHklP (HklPath qp samp) det = skipMalformed $ forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withQxQyQzPath f det qp $ \getDataFrameQxQyQz ->
      withSamplePathP f samp $ \getSample ->
      forM_ [from..to-1] (\j -> tryYield ( DataFrameHkl
                                          <$> getDataFrameQxQyQz j
                                          <*> getSample
                                        ))
