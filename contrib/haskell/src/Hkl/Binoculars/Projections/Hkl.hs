{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.Hkl
    ( Config(..)
    , DataFrameHkl(..)
    , DataPath(..)
    , SamplePath(..)
    , defaultDataPathHkl
    , newHkl
    , processHkl
    , spaceHkl
    , updateHkl
    ) where


import           Bindings.HDF5.Core                (Location)
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Lens                      (makeLenses)
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger, logDebug,
                                                    logDebugSH, logErrorSH,
                                                    logInfo)
import           Control.Monad.Reader              (MonadReader, ask, forM_,
                                                    forever)
import           Control.Monad.Trans.Reader        (runReaderT)
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    eitherDecode', encode)
import           Data.Array.Repa                   (Array, Z)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import           Data.Ini.Config.Bidir             (FieldValue (..), field, ini,
                                                    section, serializeIni, (.=),
                                                    (.=?))
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (pack)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           Data.Text.IO                      (putStr)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           GHC.Conc                          (getNumCapabilities)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~), (/~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (Pipe, await, each,
                                                    runEffect, (>->))
import           Pipes.Prelude                     (map, tee, toListM)
import           Pipes.Safe                        (MonadSafe, runSafeP,
                                                    runSafeT)
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QxQyQz
import           Hkl.C.Binoculars
import           Hkl.C.Sample
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Types


----------------
-- Exceptions --
----------------

data HklBinocularsProjectionsHklException
    = MissingSampleParameters (Config 'HklProjection)
    deriving (Show)

instance Exception HklBinocularsProjectionsHklException

----------------
-- DataPath's --
----------------

data SamplePath
    = SamplePath
      (Hdf5Path Z Double) -- a
      (Hdf5Path Z Double) -- b
      (Hdf5Path Z Double) -- c
      (Hdf5Path Z Double) -- alpha
      (Hdf5Path Z Double) -- beta
      (Hdf5Path Z Double) -- gamma
      (Hdf5Path Z Double) -- ux
      (Hdf5Path Z Double) -- uy
      (Hdf5Path Z Double) -- yz
    | SamplePath2 Sample
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

defaultDataPathHklSample :: SamplePath
defaultDataPathHklSample = SamplePath
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/A")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/B")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/C")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Alpha")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Beta")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Gamma")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Ux")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Uy")
  (hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Uz")

data instance DataPath 'HklProjection = DataPathHkl
  { dataPathHklQxQyQz :: DataPath 'QxQyQzProjection
  , dataPathHklSample :: SamplePath
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

defaultDataPathHkl :: DataPath 'HklProjection
defaultDataPathHkl = DataPathHkl
  { dataPathHklQxQyQz = defaultDataPathQxQyQz
  , dataPathHklSample = defaultDataPathHklSample
  }

instance HasFieldValue (DataPath 'HklProjection) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------



data instance Config 'HklProjection = BinocularsConfigHkl
  { _binocularsConfigHklNcore                  :: Maybe Int
  , _binocularsConfigHklDestination            :: DestinationTmpl
  , _binocularsConfigHklOverwrite              :: Bool
  , _binocularsConfigHklInputType              :: InputType
  , _binocularsConfigHklNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigHklTmpl                   :: Maybe InputTmpl
  , _binocularsConfigHklInputRange             :: Maybe ConfigRange
  , _binocularsConfigHklDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigHklCentralpixel           :: (Int, Int)
  , _binocularsConfigHklSdd                    :: Meter
  , _binocularsConfigHklDetrot                 :: Maybe Degree
  , _binocularsConfigHklAttenuationCoefficient :: Maybe Double
  , _binocularsConfigHklMaskmatrix             :: Maybe MaskLocation
  , _binocularsConfigHklA                      :: Maybe Angstrom
  , _binocularsConfigHklB                      :: Maybe Angstrom
  , _binocularsConfigHklC                      :: Maybe Angstrom
  , _binocularsConfigHklAlpha                  :: Maybe Degree
  , _binocularsConfigHklBeta                   :: Maybe Degree
  , _binocularsConfigHklGamma                  :: Maybe Degree
  , _binocularsConfigHklUx                     :: Maybe Degree
  , _binocularsConfigHklUy                     :: Maybe Degree
  , _binocularsConfigHklUz                     :: Maybe Degree
  , _binocularsConfigHklWavelength             :: Maybe Angstrom
  , _binocularsConfigHklProjectionType         :: ProjectionType
  , _binocularsConfigHklProjectionResolution   :: [Double]
  , _binocularsConfigHklProjectionLimits       :: Maybe [Limits]
  , _binocularsConfigHklDataPath               :: Maybe (DataPath 'HklProjection)
  } deriving (Eq, Show)

makeLenses 'BinocularsConfigHkl

instance HasIniConfig 'HklProjection where
  defaultConfig = BinocularsConfigHkl
    { _binocularsConfigHklNcore = Nothing
    , _binocularsConfigHklDestination = DestinationTmpl "."
    , _binocularsConfigHklOverwrite = False
    , _binocularsConfigHklInputType = SixsFlyScanUhv
    , _binocularsConfigHklNexusdir = Nothing
    , _binocularsConfigHklTmpl = Nothing
    , _binocularsConfigHklInputRange  = Nothing
    , _binocularsConfigHklDetector = Nothing
    , _binocularsConfigHklCentralpixel = (0, 0)
    , _binocularsConfigHklSdd = Meter (1 *~ meter)
    , _binocularsConfigHklDetrot = Nothing
    , _binocularsConfigHklAttenuationCoefficient = Nothing
    , _binocularsConfigHklMaskmatrix = Nothing
    , _binocularsConfigHklA  = Nothing
    , _binocularsConfigHklB = Nothing
    , _binocularsConfigHklC = Nothing
    , _binocularsConfigHklAlpha  = Nothing
    , _binocularsConfigHklBeta = Nothing
    , _binocularsConfigHklGamma  = Nothing
    , _binocularsConfigHklUx = Nothing
    , _binocularsConfigHklUy = Nothing
    , _binocularsConfigHklUz = Nothing
    , _binocularsConfigHklWavelength = Nothing
    , _binocularsConfigHklProjectionType = HklProjection
    , _binocularsConfigHklProjectionResolution = [0.01, 0.01, 0.01]
    , _binocularsConfigHklProjectionLimits  = Nothing
    , _binocularsConfigHklDataPath = Just defaultDataPathHkl
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigHklNcore .=? field "ncores" auto
      binocularsConfigHklDestination .= field "destination" auto
      binocularsConfigHklOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigHklInputType .= field "type" auto
      binocularsConfigHklNexusdir .=? field "nexusdir" auto
      binocularsConfigHklTmpl .=? field "inputtmpl" auto
      binocularsConfigHklInputRange .=? field "inputrange" auto
      binocularsConfigHklDetector .=? field "detector" auto
      binocularsConfigHklCentralpixel .= field "centralpixel" auto
      binocularsConfigHklSdd .= field "sdd" auto
      binocularsConfigHklDetrot .=? field "detrot" auto
      binocularsConfigHklAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigHklMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigHklA .=? field "a" auto
      binocularsConfigHklB .=? field "b" auto
      binocularsConfigHklC .=? field "c" auto
      binocularsConfigHklAlpha .=?field "alpha" auto
      binocularsConfigHklBeta .=? field "beta" auto
      binocularsConfigHklGamma .=? field "gamma" auto
      binocularsConfigHklUx .=? field "ux" auto
      binocularsConfigHklUy .=? field "uy" auto
      binocularsConfigHklUz .=? field "uz" auto
      binocularsConfigHklWavelength .=? field "wavelength" auto
      binocularsConfigHklDataPath .=? field "datapath" auto
    section "projection" $ do
      binocularsConfigHklProjectionType .= field "type" auto
      binocularsConfigHklProjectionResolution .= field "resolution" auto
      binocularsConfigHklProjectionLimits .=? field "limits" auto

  overwriteInputRange mr c = case mr of
                               Nothing  -> c
                               (Just _) -> c{_binocularsConfigHklInputRange = mr}


overloadSampleWithConfig :: (Config 'HklProjection) -> Sample -> Sample
overloadSampleWithConfig conf (Sample
                               name
                               (Triclinic a b c alpha beta gamma)
                               ux uy uz) =
    Sample name nlat nux nuy nuz
        where
          nlat = Triclinic
                 (maybe a (NanoMeter . unAngstrom) (_binocularsConfigHklA conf))
                 (maybe b (NanoMeter . unAngstrom) (_binocularsConfigHklB conf))
                 (maybe c (NanoMeter . unAngstrom) (_binocularsConfigHklC conf))
                 (maybe alpha id (_binocularsConfigHklAlpha conf))
                 (maybe beta id (_binocularsConfigHklBeta conf))
                 (maybe gamma id (_binocularsConfigHklGamma conf))

          go :: Parameter -> Maybe Double -> Parameter
          go p@(Parameter _ v _) nv = p{parameterValue=fromMaybe v nv}

          nux = go ux ((/~ degree) . unDegree <$> _binocularsConfigHklUx conf)
          nuy = go uy ((/~ degree) . unDegree <$> _binocularsConfigHklUy conf)
          nuz = go uz ((/~ degree) . unDegree <$> _binocularsConfigHklUz conf)

getResolution' :: MonadThrow m => (Config 'HklProjection) -> m [Double]
getResolution' c = getResolution (_binocularsConfigHklProjectionResolution c) 3

sampleConfig ::  (Config 'HklProjection) -> Maybe Sample
sampleConfig cf = do
  (Angstrom a) <- _binocularsConfigHklA cf
  (Angstrom b) <- _binocularsConfigHklB cf
  (Angstrom c) <- _binocularsConfigHklC cf
  alpha <- _binocularsConfigHklAlpha cf
  beta <- _binocularsConfigHklBeta cf
  gamma <- _binocularsConfigHklGamma cf
  (Degree ux) <- _binocularsConfigHklUx cf
  (Degree uy) <- _binocularsConfigHklUy cf
  (Degree uz) <- _binocularsConfigHklUz cf
  let pux = Parameter "ux" (ux /~ degree) (Range 0  360)
  let puy = Parameter "uy" (uy /~ degree) (Range 0  360)
  let puz = Parameter "uz" (uz /~ degree) (Range 0  360)
  return $ Sample "triclinic" (Triclinic (NanoMeter a) (NanoMeter b) (NanoMeter c)
                                alpha beta gamma) pux puy puz

----------------
-- Projection --
----------------

data DataFrameHkl a
    = DataFrameHkl DataFrameQxQyQz Sample
      deriving Show


{-# INLINE spaceHkl #-}
spaceHkl :: (Config 'HklProjection) -> Detector b DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> Maybe [Limits] -> Space DIM3 -> DataFrameHkl b -> IO (DataFrameSpace DIM3)
spaceHkl config' det pixels rs mmask' mlimits space@(Space fSpace) (DataFrameHkl (DataFrameQxQyQz _ att g img) samp) = do
  let sample' = overloadSampleWithConfig config' samp
  withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withSample sample' $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withMaybeMask mmask' $ \ mask'' ->
    withPixelsDims pixels $ \ndim dims ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withForeignPtr fSpace $ \pSpace -> do
    case img of
      (ImageInt32 fp) -> withForeignPtr fp $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_int32_t" #-} c'hkl_binoculars_space_hkl_int32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
      (ImageWord16 fp) -> withForeignPtr fp $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint16_t" #-} c'hkl_binoculars_space_hkl_uint16_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
      (ImageWord32 fp) -> withForeignPtr fp $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint32_t" #-} c'hkl_binoculars_space_hkl_uint32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
    return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class ChunkP a => FramesHklP a where
  framesHklP :: MonadSafe m
             => a -> Detector b DIM2 -> Pipe (FilePath, [Int]) (DataFrameHkl b) m ()


class (FramesHklP a, Show a) => ProcessHklP a where
  processHklP :: (MonadIO m, MonadLogger m, MonadReader (Config 'HklProjection) m, MonadThrow m)
              => m a -> m ()
  processHklP mkPaths = do
    conf :: Config 'HklProjection <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigHklDetector conf)
    let mlimits = _binocularsConfigHklProjectionLimits conf
    let destination = _binocularsConfigHklDestination conf
    let output' = case _binocularsConfigHklInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> destination' (ConfigRange []) mlimits destination
    let centralPixel' = _binocularsConfigHklCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigHklSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigHklDetrot conf)

    filenames <- InputList
                <$> files (_binocularsConfigHklNexusdir conf)
                          (_binocularsConfigHklInputRange conf)
                          (_binocularsConfigHklTmpl conf)
    mask' <- getMask (_binocularsConfigHklMaskmatrix conf) det
    res <- getResolution' conf
    h5d <- mkPaths
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

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
      >-> framesHklP h5d det
      >-> project det 3 (spaceHkl conf det pixels res mask' mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    $(logInfo) (pack $ printf "let's do an Hkl projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runEffect $ runSafeP $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               -- >-> tee Pipes.Prelude.print
                               >-> framesHklP h5d det
                               -- >-> filter (\(DataFrameHkl (DataFrameQxQyQz _ _ _ ma) _) -> isJust ma)
                               >-> project det 3 (spaceHkl conf det pixels res mask' mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'

instance ProcessHklP (DataPath 'HklProjection)

-- FramesHklP

withSamplePathP :: (MonadSafe m, Location l) => l -> SamplePath -> (IO Sample -> m r) -> m r
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
                <$> extract1DStreamValue a' 0
                <*> extract1DStreamValue b' 0
                <*> extract1DStreamValue c' 0
                <*> extract1DStreamValue alpha' 0
                <*> extract1DStreamValue beta' 0
                <*> extract1DStreamValue gamma' 0)
           <*> (Parameter "ux"
                <$> extract1DStreamValue ux' 0
                <*> pure (Range 0 0))
           <*> (Parameter "uy"
                <$> extract1DStreamValue uy' 0
                <*> pure (Range 0 0))
           <*> (Parameter "uz"
                <$> extract1DStreamValue uz' 0
                <*> pure (Range 0 0)))
withSamplePathP _ (SamplePath2 s) g = g (return s)

instance ChunkP (DataPath 'HklProjection) where
  chunkP (DataPathHkl p _)           = chunkP p

instance FramesHklP (DataPath 'HklProjection) where
  framesHklP (DataPathHkl qp samp) det = skipMalformed $ forever $ do
    (fp, js) <- await
    withFileP (openH5 fp) $ \f ->
      withDataPathQxQyQz f det qp $ \getDataFrameQxQyQz ->
      withSamplePathP f samp $ \getSample ->
      forM_ js (\j -> tryYield ( DataFrameHkl
                                <$> getDataFrameQxQyQz j
                                <*> getSample
                              ))

------------
-- Inputs --
------------

h5dpathHkl :: (MonadLogger m, MonadThrow m)
           => (Config 'HklProjection)
           -> m (DataPath 'HklProjection)
h5dpathHkl c =
  do let i = _binocularsConfigHklInputType c
     let ma = _binocularsConfigHklAttenuationCoefficient c
     let samplePath beamline device =
           SamplePath
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "A")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "B")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "C")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Ux")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uy")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uz")
     let sampleMarsPath beamline device =
           SamplePath
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "a")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "b")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "c")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_x")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_y")
           (hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_z")
     let marsSamplePath = sampleMarsPath "MARS" "d03-1-cx2__ex__dif-cm_#1"
     let medHSamplePath = samplePath "SIXS" "i14-c-cx1-ex-cm-med.h"
     let medVSamplePath = samplePath "SIXS" "i14-c-cx1-ex-cm-med.v"
     let uhvSamplePath  = samplePath "SIXS" "I14-C-CX2__EX__DIFF-UHV__#1"
     let uhvSamplePath2 = samplePath "SIXS" "i14-c-cx2-ex-diff-uhv"
     let uhvSamplePath3 = samplePath "SIXS" "i14-c-cx2-ex-cm-uhv"
     qxqyqz <- h5dpathQxQyQz i ma
     case i of
       CristalK6C -> do
         let ms = sampleConfig c
         case ms of
           (Just s) -> return (DataPathHkl qxqyqz (SamplePath2 s))
           Nothing  -> throwM (MissingSampleParameters c)
       MarsFlyscan -> return $ DataPathHkl qxqyqz marsSamplePath
       MarsSbs -> return $ DataPathHkl qxqyqz marsSamplePath
       SixsFlyMedH -> return $ DataPathHkl qxqyqz medHSamplePath
       SixsFlyMedV -> return $ DataPathHkl qxqyqz medVSamplePath
       SixsFlyMedVEiger -> return $ DataPathHkl qxqyqz medVSamplePath
       SixsFlyMedVS70 -> return $ DataPathHkl qxqyqz medVSamplePath
       SixsFlyScanUhv -> return $ DataPathHkl qxqyqz uhvSamplePath
       SixsFlyScanUhv2 -> return $ DataPathHkl qxqyqz uhvSamplePath3
       SixsFlyScanUhvTest -> return $ DataPathHkl qxqyqz uhvSamplePath3
       SixsFlyScanUhvUfxc -> return $ DataPathHkl qxqyqz uhvSamplePath
       SixsSbsFixedDetector -> undefined -- TODO this must not be possible.
       SixsSbsMedH -> return $ DataPathHkl qxqyqz medHSamplePath
       SixsSbsMedV -> return $ DataPathHkl qxqyqz medVSamplePath
       SixsSbsMedVFixDetector -> return $ DataPathHkl qxqyqz medVSamplePath

         -- SixsSbsMedV -> DataPathHkl
         --               hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")  -- xpad
         --               (GeometryPath
         --                hdf5p -- TODO wavelength
         --                [ hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch" -- beta
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")
         --                ])
         --               medVSamplePath
         --               -- "attenuation": DatasetPathWithAttribute("long_name", b"i14-c-c00/ex/roic/att"),
         --               -- "timestamp": HItem("sensors_timestamps", True),

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'HklProjection) m)
         => m ()
process' = do
  c <- ask
  processHklP (h5dpathHkl c)

processHkl :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
processHkl mf mr = do
  econf <- liftIO $ getConfig mf
  case econf of
    Right conf -> do
      $(logDebug) "config red from the config file"
      $(logDebugSH) conf
      let conf' = overwriteInputRange mr conf
      $(logDebug) "config once overloaded with the command line arguments"
      $(logDebugSH) conf'
      runReaderT process' conf'
    Left e      -> $(logErrorSH) e

newHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newHkl cwd = do
  let conf = defaultConfig {_binocularsConfigHklNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)


updateHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> m ()
updateHkl mf = do
  (conf :: Either String (Config 'HklProjection)) <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)
