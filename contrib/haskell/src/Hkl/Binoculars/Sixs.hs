{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Sixs
  ( new
  , process
  , update
  ) where

import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Logger               (MonadLogger, logDebugN)
import           Path.IO                            (getCurrentDir)
import           Path.Posix                         (parseAbsDir)

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections.Angles
import           Hkl.Binoculars.Projections.Hkl
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.Utils

process :: (MonadLogger m, MonadThrow m, MonadIO m)
        => Maybe FilePath -> Maybe ConfigRange -> m ()
process mf mr = do
  epreconf <- liftIO $ getPreConfig mf
  logDebugN "pre-config red from the config file"
  logDebugNSH epreconf
  case epreconf of
    Left e        -> logErrorNSH e
    Right preconf -> case _binocularsPreConfigProjectionType preconf of
                      AnglesProjection   -> processAngles mf mr
                      Angles2Projection  -> processAngles mf mr
                      HklProjection      -> processHkl mf mr
                      QCustomProjection  -> processQCustom mf mr
                      QIndexProjection   -> processQCustom mf mr
                      QparQperProjection -> processQCustom mf mr
                      QxQyQzProjection   -> processQCustom mf mr

new :: (MonadIO m, MonadLogger m, MonadThrow m)
    => ProjectionType -> Maybe FilePath -> m ()
new p mf = do
  cwd <- case mf of
          (Just f) -> parseAbsDir f
          Nothing  -> getCurrentDir
  case p of
    AnglesProjection   -> newAngles cwd
    Angles2Projection  -> newAngles cwd
    HklProjection      -> newHkl cwd
    QCustomProjection  -> newQCustom cwd
    QIndexProjection   -> newQCustom cwd
    QparQperProjection -> newQCustom cwd
    QxQyQzProjection   -> newQCustom cwd

update :: (MonadIO m, MonadLogger m, MonadThrow m) => FilePath -> m ()
update f = do
  epreconf <- liftIO $ getPreConfig (Just f)
  logDebugN "pre-config red from the config file"
  logDebugNSH epreconf
  case epreconf of
    Left e        -> logErrorNSH e
    Right preconf -> case _binocularsPreConfigProjectionType preconf of
                      AnglesProjection   -> updateAngles (Just f)
                      Angles2Projection  -> updateAngles (Just f)
                      HklProjection      -> updateHkl (Just f)
                      QCustomProjection  -> updateQCustom (Just f)
                      QIndexProjection   -> updateQCustom (Just f)
                      QparQperProjection -> updateQCustom (Just f)
                      QxQyQzProjection   -> updateQCustom (Just f)
