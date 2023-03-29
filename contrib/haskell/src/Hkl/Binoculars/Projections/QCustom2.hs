{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-splices #-}
{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QCustom2
    ( newQCustom2
    , processQCustom2
    , updateQCustom2
    ) where

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.Array.Repa                    (Array)
import           Data.Array.Repa.Index              (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr    (F, toForeignPtr)
import           Data.HashMap.Lazy                  (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Maybe                         (fromJust)
import           Data.Text                          (pack, unpack)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           GHC.Generics                       (Generic)
import           Generic.Random                     (genericArbitraryU)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (each, runEffect, (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (runSafeT)
import           Test.QuickCheck                    (Arbitrary (..))
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Config.Common
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.Image
import           Hkl.Orphan                         ()
import           Hkl.Types
import           Hkl.Utils

-------------------------
-- QCustom2 Projection --
-------------------------

------------
-- Config --
------------

data instance Config 'QCustom2Projection
  = BinocularsConfig'QCustom2
    { binocularsConfig'QCustom2'Common :: BinocularsConfig'Common
    , binocularsConfig'QCustom2'SurfaceOrientation     :: SurfaceOrientation
    , binocularsConfig'QCustom2'ProjectionType         :: ProjectionType
    , binocularsConfig'QCustom2'ProjectionResolution   :: Resolutions DIM3
    , binocularsConfig'QCustom2'ProjectionLimits       :: Maybe (RLimits DIM3)
    , binocularsConfig'QCustom2'DataPath               :: DataSourcePath DataFrameQCustom
    , binocularsConfig'QCustom2'SubProjection          :: Maybe QCustomSubProjection
    } deriving (Show, Generic)

instance Arbitrary (Config 'QCustom2Projection) where
  arbitrary = genericArbitraryU

newtype instance Args 'QCustom2Projection = Args'QCustom2Projection (Maybe ConfigRange)

default'BinocularsConfig'QCustom2 :: Config 'QCustom2Projection
default'BinocularsConfig'QCustom2
  = BinocularsConfig'QCustom2
    { binocularsConfig'QCustom2'Common = default'BinocularsConfig'Common
    , binocularsConfig'QCustom2'SurfaceOrientation = SurfaceOrientationVertical
    , binocularsConfig'QCustom2'ProjectionType = QCustomProjection
    , binocularsConfig'QCustom2'ProjectionResolution = Resolutions3 0.01 0.01 0.01
    , binocularsConfig'QCustom2'ProjectionLimits  = Nothing
    , binocularsConfig'QCustom2'DataPath = default'DataSourcePath'DataFrameQCustom
    , binocularsConfig'QCustom2'SubProjection = Just QCustomSubProjection'QxQyQz
    }


instance HasIniConfig 'QCustom2Projection where

  getConfig (ConfigContent cfg) (Args'QCustom2Projection mr) capabilities = do
    common <- parse'BinocularsConfig'Common cfg mr capabilities
    surface_orientation <- parseFDef cfg "input" "surface_orientation" (binocularsConfig'QCustom2'SurfaceOrientation default'BinocularsConfig'QCustom2)
    let edatapath = parseMb cfg "input" "datapath"

    -- section projection
    projectiontype <- parseFDef cfg "projection" "type" (binocularsConfig'QCustom2'ProjectionType default'BinocularsConfig'QCustom2)
    resolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'QCustom2'ProjectionResolution default'BinocularsConfig'QCustom2)
    limits <- parseMb cfg "projection" "limits"
    let msubprojection = parseMbDef cfg "projection" "subprojection" (binocularsConfig'QCustom2'SubProjection default'BinocularsConfig'QCustom2)

    -- customize a bunch of parameters

    -- fix the subprojection depending on the projection type
    let subprojection = case projectiontype of
                          QIndexProjection -> Just QCustomSubProjection'QIndex
                          QparQperProjection -> Just QCustomSubProjection'QparQper
                          QxQyQzProjection -> Just QCustomSubProjection'QxQyQz
                          _                -> msubprojection

        -- compute the datatype
    let datapath = case edatapath of
                 Left _ -> guess'DataSourcePath'DataFrameQCustom common subprojection
                 Right Nothing -> guess'DataSourcePath'DataFrameQCustom common subprojection
                 Right (Just d)  -> overload'DataSourcePath'DataFrameQCustom common subprojection d

    pure $ BinocularsConfig'QCustom2 common surface_orientation projectiontype resolution limits datapath subprojection


instance ToIni (Config 'QCustom2Projection) where

  toIni c = toIni (binocularsConfig'QCustom2'Common c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input",    elemF   "surface_orientation" (binocularsConfig'QCustom2'SurfaceOrientation c)
                                                     <> elemF   "datapath" (binocularsConfig'QCustom2'DataPath c)
                                           )
                                         , ("projection",    elemF   "type" (binocularsConfig'QCustom2'ProjectionType c)
                                                          <> elemF   "resolution" (binocularsConfig'QCustom2'ProjectionResolution c)
                                                          <> elemFMb "limits" (binocularsConfig'QCustom2'ProjectionLimits c)
                                                          <> elemFMb "subprojection" (binocularsConfig'QCustom2'SubProjection c)
                                           )]

                , iniGlobals = []
                }

------------------
-- Input Path's --
------------------

{-# INLINE spaceQCustom2 #-}
spaceQCustom2 :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> SurfaceOrientation -> Maybe (RLimits DIM3) -> QCustomSubProjection -> Space DIM3 -> DataFrameQCustom -> IO (DataFrameSpace DIM3)
spaceQCustom2 det pixels rs mmask' surf mlimits subprojection space@(Space fSpace) (DataFrameQCustom att g img index) =
  withNPixels det $ \nPixels ->
  withForeignPtr g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom2_int32_t" #-} c'hkl_binoculars_space_qcustom2_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom2_uint16_t" #-} c'hkl_binoculars_space_qcustom2_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom2_uint32_t" #-} c'hkl_binoculars_space_qcustom2_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processQCustom2P :: (MonadIO m, MonadLogger m, MonadReader (Config 'QCustomProjection) m, MonadThrow m)
                => m ()
processQCustom2P = do
  (conf :: Config 'QCustomProjection) <- ask

  -- directly from the common config
  let common = binocularsConfig'QCustom'Common conf
  let overwrite = binocularsConfig'Common'Overwrite common
  let det = binocularsConfig'Common'Detector common
  let (NCores cap) =  binocularsConfig'Common'NCores common
  let destination = binocularsConfig'Common'Destination common
  let centralPixel' = binocularsConfig'Common'Centralpixel common
  let (Meter sampleDetectorDistance) = binocularsConfig'Common'Sdd common
  let (Degree detrot) = binocularsConfig'Common'Detrot common
  let mImageSumMax = binocularsConfig'Common'ImageSumMax common
  let inputRange = binocularsConfig'Common'InputRange common
  let nexusDir = binocularsConfig'Common'Nexusdir common
  let tmpl = binocularsConfig'Common'Tmpl common
  let maskMatrix = binocularsConfig'Common'Maskmatrix common

  -- directly from the specific config
  let mlimits = binocularsConfig'QCustom'ProjectionLimits conf
  let res = binocularsConfig'QCustom'ProjectionResolution conf
  let surfaceOrientation = binocularsConfig'QCustom'SurfaceOrientation conf
  let datapaths = binocularsConfig'QCustom'DataPath conf
  let subprojection = fromJust (binocularsConfig'QCustom'SubProjection conf) -- should not be Maybe
  let projectionType = binocularsConfig'QCustom'ProjectionType conf

  -- built from the config
  output' <- liftIO $ destination' projectionType inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir (Just inputRange) tmpl
  mask' <- getMask maskMatrix det
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot NoNormalisation

  -- compute the jobs

  let fns = concatMap (replicate 1) (toList filenames)
  chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP datapaths
  let ntot = sum (Prelude.map clength chunks)
  let jobs = chunk (quot ntot cap) chunks

  -- log parameters

  logDebugNSH filenames
  logDebugNSH datapaths
  logDebugNSH chunks
  logDebugN "start gessing final cube size"

  -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

  guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
    runSafeT $ runEffect $
    each chunks
    >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t]))
    >-> framesQCustomP datapaths
    >-> project det 3 (spaceQCustom2 det pixels res mask' surfaceOrientation mlimits subprojection)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  -- do the final projection

  logInfoN $ pack $ printf "let's do a QCustom projection of %d %s image(s) on %d core(s)" ntot (show det) cap

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             >-> framesQCustomP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceQCustom2 det pixels res mask' surfaceOrientation mlimits subprojection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

---------
-- Cmd --
---------

processQCustom2 :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQCustom2 mf mr = cmd processQCustom2P mf (Args'QCustomProjection mr)

newQCustom2 :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Path Abs Dir -> m ()
newQCustom2 cwd = do
  let conf = default'BinocularsConfig'QCustom2
             { binocularsConfig'QCustom2'Common = default'BinocularsConfig'Common
                                                  { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateQCustom2 :: (MonadIO m, MonadLogger m, MonadThrow m)
              => Maybe FilePath -> Maybe ConfigRange -> m ()
updateQCustom2 mf mr = cmd (pure ()) mf (Args'QCustomProjection mr)
