{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QparQper
    ( newQparQper
    , processQparQper
    , updateQparQper
    ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Lens                      (makeLenses)
import           Control.Monad.Catch               (MonadThrow)
import           Control.Monad.IO.Class            (MonadIO (liftIO), liftIO)
import           Control.Monad.Logger              (MonadLogger, logDebug,
                                                    logDebugSH, logErrorSH,
                                                    logInfo)
import           Control.Monad.Reader              (MonadReader, ask)
import           Control.Monad.Trans.Reader        (runReaderT)
import           Data.Array.Repa                   (Array)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Either.Extra                 (mapRight)
import           Data.Ini.Config.Bidir             (IniSpec, field, getIniValue,
                                                    ini, parseIni, section,
                                                    serializeIni, (.=), (.=?))
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (pack)
import           Data.Text.IO                      (putStr)
import           Data.Typeable                     (typeOf)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           GHC.Conc                          (getNumCapabilities)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (Pipe, each, runEffect,
                                                    (>->))
import           Pipes.Prelude                     (map, tee, toListM)
import           Pipes.Safe                        (MonadSafe, runSafeT)
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QxQyQz
import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.Image

data BinocularsConfigQparQper = BinocularsConfigQparQper
  { _binocularsConfigQparQperNcore                  :: Maybe Int
  , _binocularsConfigQparQperDestination            :: DestinationTmpl
  , _binocularsConfigQparQperOverwrite              :: Bool
  , _binocularsConfigQparQperInputType              :: InputType
  , _binocularsConfigQparQperNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigQparQperTmpl                   :: Maybe InputTmpl
  , _binocularsConfigQparQperInputRange             :: Maybe ConfigRange
  , _binocularsConfigQparQperDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigQparQperCentralpixel           :: (Int, Int)
  , _binocularsConfigQparQperSdd                    :: Meter
  , _binocularsConfigQparQperDetrot                 :: Maybe Degree
  , _binocularsConfigQparQperAttenuationCoefficient :: Maybe Double
  , _binocularsConfigQparQperSurfaceOrientation     :: Maybe SurfaceOrientation
  , _binocularsConfigQparQperMaskmatrix             :: Maybe MaskLocation
  , _binocularsConfigQparQperWavelength             :: Maybe Angstrom
  , _binocularsConfigQparQperProjectionType         :: ProjectionType
  , _binocularsConfigQparQperProjectionResolution   :: [Double]
  , _binocularsConfigQparQperProjectionLimits       :: Maybe [Limits]
  } deriving (Eq, Show)

makeLenses ''BinocularsConfigQparQper

binocularsConfigQparQperDefault :: BinocularsConfigQparQper
binocularsConfigQparQperDefault = BinocularsConfigQparQper
  { _binocularsConfigQparQperNcore = Nothing
  , _binocularsConfigQparQperDestination = DestinationTmpl "."
  , _binocularsConfigQparQperOverwrite = False
  , _binocularsConfigQparQperInputType = SixsFlyScanUhv
  , _binocularsConfigQparQperNexusdir = Nothing
  , _binocularsConfigQparQperTmpl = Nothing
  , _binocularsConfigQparQperInputRange  = Nothing
  , _binocularsConfigQparQperDetector = Nothing
  , _binocularsConfigQparQperCentralpixel = (0, 0)
  , _binocularsConfigQparQperSdd = Meter (1 *~ meter)
  , _binocularsConfigQparQperDetrot = Nothing
  , _binocularsConfigQparQperAttenuationCoefficient = Nothing
  , _binocularsConfigQparQperSurfaceOrientation = Just SurfaceOrientationVertical
  , _binocularsConfigQparQperMaskmatrix = Nothing
  , _binocularsConfigQparQperWavelength = Nothing
  , _binocularsConfigQparQperProjectionType = QparQperProjection
  , _binocularsConfigQparQperProjectionResolution = [0.01, 0.01]
  , _binocularsConfigQparQperProjectionLimits  = Nothing
  }

binocularsConfigQparQperSpec :: IniSpec BinocularsConfigQparQper ()
binocularsConfigQparQperSpec = do
  section "dispatcher" $ do
    binocularsConfigQparQperNcore .=? field "ncores" auto
    binocularsConfigQparQperDestination .= field "destination" auto
    binocularsConfigQparQperOverwrite .= field "overwrite" auto
  section "input" $ do
    binocularsConfigQparQperInputType .= field "type" auto
    binocularsConfigQparQperNexusdir .=? field "nexusdir" auto
    binocularsConfigQparQperTmpl .=? field "inputtmpl" auto
    binocularsConfigQparQperInputRange .=? field "inputrange" auto
    binocularsConfigQparQperDetector .=? field "detector" auto
    binocularsConfigQparQperCentralpixel .= field "centralpixel" auto
    binocularsConfigQparQperSdd .= field "sdd" auto
    binocularsConfigQparQperDetrot .=? field "detrot" auto
    binocularsConfigQparQperAttenuationCoefficient .=? field "attenuation_coefficient" auto
    binocularsConfigQparQperSurfaceOrientation .=? field "surface_orientation" auto
    binocularsConfigQparQperMaskmatrix .=? field "maskmatrix" auto
    binocularsConfigQparQperWavelength .=? field "wavelength" auto
  section "projection" $ do
    binocularsConfigQparQperProjectionType .= field "type" auto
    binocularsConfigQparQperProjectionResolution .= field "resolution" auto
    binocularsConfigQparQperProjectionLimits .=? field "limits" auto

-------------------------
-- QparQper Projection --
-------------------------

newtype QparQperPath = QparQperPath QxQyQzPath

instance Show QparQperPath where
  show = show . typeOf

newtype DataFrameQparQper = DataFrameQparQper DataFrameQxQyQz

{-# INLINE spaceQparQper #-}
spaceQparQper :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> SurfaceOrientation -> Maybe [Limits] -> Space DIM2 -> DataFrameQparQper -> IO (DataFrameSpace DIM2)
spaceQparQper det pixels rs mmask' surf mlimits space@(Space fSpace) (DataFrameQparQper (DataFrameQxQyQz _ att g img)) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withArrayLen rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_qparqper_int32_t" #-} c'hkl_binoculars_space_qparqper_int32_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord16 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_qparqper_uint16_t" #-} c'hkl_binoculars_space_qparqper_uint16_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord32 fp) -> withForeignPtr fp $ \i -> do
      {-# SCC "hkl_binoculars_space_qparqper_uint32_t" #-} c'hkl_binoculars_space_qparqper_uint32_t pSpace geometry i nPixels (CDouble att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

getResolution' :: MonadThrow m => BinocularsConfigQparQper -> m [Double]
getResolution' c = getResolution (_binocularsConfigQparQperProjectionResolution c) 2

class ChunkP a => FramesQparQperP a where
  framesQparQperP :: MonadSafe m
                  => a -> Detector b DIM2 -> Pipe (FilePath, [Int]) DataFrameQparQper m ()

class (FramesQparQperP a, Show a) => ProcessQparQperP a where
  processQparQperP :: (MonadIO m, MonadLogger m, MonadReader BinocularsConfigQparQper m, MonadThrow m)
                   => m a -> m ()
  processQparQperP mkPaths = do
    (conf :: BinocularsConfigQparQper) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigQparQperDetector conf)
    let output' = case _binocularsConfigQparQperInputRange conf of
                   Just r  -> destination' r (_binocularsConfigQparQperDestination conf)
                   Nothing -> destination' (ConfigRange []) (_binocularsConfigQparQperDestination conf)
    let centralPixel' = _binocularsConfigQparQperCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQparQperSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQparQperDetrot conf)
    let surfaceOrientation = fromMaybe SurfaceOrientationVertical (_binocularsConfigQparQperSurfaceOrientation conf)
    let mlimits = _binocularsConfigQparQperProjectionLimits conf

    h5d <- mkPaths
    filenames <- InputList
                <$> files (_binocularsConfigQparQperNexusdir conf)
                          (_binocularsConfigQparQperInputRange conf)
                          (_binocularsConfigQparQperTmpl conf)
    mask' <- getMask (_binocularsConfigQparQperMaskmatrix conf) det
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot
    res <- getResolution' conf

    -- compute the jobs

    let fns = concatMap (replicate 1) (toList filenames)
    chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP h5d
    cap' <-  liftIO $ getNumCapabilities
    let ntot = sum (Prelude.map clength chunks)
    let cap = if cap' >= 2 then cap' - 1 else cap'
    let jobs = chunk (quot ntot cap) chunks

    -- log parameters

    $(logDebugSH) filenames
    $(logDebugSH) h5d
    $(logDebug) "start gessing final cube size"

    -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

    guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
      runSafeT $ runEffect $
      each chunks
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, (quot (f + t) 4), (quot (f + t) 4) * 2, (quot (f + t) 4) * 3, t]))
      >-> framesQparQperP h5d det
      >-> project det 2 (spaceQparQper det pixels res mask' surfaceOrientation mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a QparQper projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQparQperP h5d det
                               -- >-> filter (\(DataFrameQxQyQz _ _ _ ma) -> isJust ma)
                               >-> project det 2 (spaceQparQper det pixels res mask' surfaceOrientation mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'

instance ProcessQparQperP QparQperPath

instance ChunkP QparQperPath where
  chunkP (QparQperPath p) = chunkP p

instance FramesQparQperP QparQperPath where
  framesQparQperP (QparQperPath qxqyqz) det = framesQxQyQzP qxqyqz det
                                              >->  Pipes.Prelude.map DataFrameQparQper

instance FramesQxQyQzP QparQperPath where
  framesQxQyQzP (QparQperPath p) det = framesQxQyQzP p det

h5dpathQparQper :: (MonadLogger m, MonadThrow m)
                => InputType
                -> Maybe Double
                -> m QparQperPath
h5dpathQparQper i ma = QparQperPath <$> (h5dpathQxQyQz i ma)


---------
-- Cmd --
---------

getConfig' :: ConfigContent -> Either String BinocularsConfigQparQper
getConfig'  (ConfigContent cfg) = do
  let r = parseIni cfg (ini binocularsConfigQparQperDefault binocularsConfigQparQperSpec)
  mapRight getIniValue r

getConfig :: Maybe FilePath -> IO (Either String BinocularsConfigQparQper)
getConfig mf = getConfig' <$> readConfig mf

combineWithCmdLineArgs :: BinocularsConfigQparQper -> Maybe ConfigRange -> BinocularsConfigQparQper
combineWithCmdLineArgs c mr = case mr of
                                Nothing  -> c
                                (Just _) -> c{_binocularsConfigQparQperInputRange = mr}

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader BinocularsConfigQparQper m)
         => m ()
process' = do
  c <- ask
  $(logDebug) "config once overloaded with the command line arguments"
  $(logDebugSH) c
  processQparQperP (h5dpathQparQper (_binocularsConfigQparQperInputType c) (_binocularsConfigQparQperAttenuationCoefficient c))

processQparQper :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
processQparQper mf mr = do
  conf <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Right conf' -> runReaderT process' (combineWithCmdLineArgs conf' mr)
    Left e      -> $(logErrorSH) e

newQparQper :: (MonadIO m, MonadLogger m, MonadThrow m)
            => Path Abs Dir -> m ()
newQparQper cwd = do
  let conf = binocularsConfigQparQperDefault {_binocularsConfigQparQperNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf binocularsConfigQparQperSpec)

updateQparQper :: (MonadIO m, MonadLogger m, MonadThrow m)
               => Maybe FilePath -> m ()
updateQparQper mf = do
  conf <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' binocularsConfigQparQperSpec)
