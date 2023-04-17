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
module Hkl.Binoculars.Command
  ( new
  , process
  , update
  ) where

import           Control.Monad.Catch                 (MonadThrow)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Logger                (LoggingT, MonadLogger,
                                                      logDebugN)
import           Path.IO                             (getCurrentDir)
import           Path.Posix                          (parseAbsDir)

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections.Angles
import           Hkl.Binoculars.Projections.Hkl
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.Binoculars.Projections.QCustom2
import           Hkl.Utils

{-# SPECIALIZE process :: Maybe FilePath -> Maybe ConfigRange ->LoggingT IO () #-}
process :: (MonadLogger m, MonadThrow m, MonadIO m)
        => Maybe FilePath -> Maybe ConfigRange -> m ()
process mf mr = do
  epreconf <- liftIO $ getPreConfig mf
  logDebugN "pre-config red from the config file"
  logDebugNSH epreconf
  case epreconf of
    Left e        -> logErrorNSH e
    Right preconf -> case _binocularsPreConfigProjectionType preconf of
                      AnglesProjection    -> processAngles mf mr
                      Angles2Projection   -> processAngles mf mr
                      HklProjection       -> processHkl mf mr
                      QCustomProjection   -> processQCustom mf mr
                      QCustom2Projection  -> processQCustom2 mf mr
                      QIndexProjection    -> processQCustom mf mr
                      QparQperProjection  -> processQCustom mf mr
                      QxQyQzProjection    -> processQCustom mf mr
                      RealSpaceProjection -> processQCustom2 mf mr
                      PixelsProjection    -> processQCustom2 mf mr

new :: (MonadIO m, MonadLogger m, MonadThrow m)
    => ProjectionType -> Maybe FilePath -> m ()
new p mf = do
  cwd <- case mf of
          (Just f) -> parseAbsDir f
          Nothing  -> getCurrentDir
  case p of
    AnglesProjection    -> newAngles cwd
    Angles2Projection   -> newAngles cwd
    HklProjection       -> newHkl cwd
    QCustomProjection   -> newQCustom cwd
    QCustom2Projection  -> newQCustom2 cwd
    QIndexProjection    -> newQCustom cwd
    QparQperProjection  -> newQCustom cwd
    QxQyQzProjection    -> newQCustom cwd
    RealSpaceProjection -> newQCustom2 cwd
    PixelsProjection    -> newQCustom2 cwd

update :: (MonadIO m, MonadLogger m, MonadThrow m) => FilePath -> Maybe ConfigRange -> m ()
update f mr = do
  epreconf <- liftIO $ getPreConfig (Just f)
  logDebugN "pre-config red from the config file"
  logDebugNSH epreconf
  case epreconf of
    Left e        -> logErrorNSH e
    Right preconf -> case _binocularsPreConfigProjectionType preconf of
                      AnglesProjection    -> updateAngles (Just f) mr
                      Angles2Projection   -> updateAngles (Just f) mr
                      HklProjection       -> updateHkl (Just f) mr
                      QCustomProjection   -> updateQCustom (Just f) mr
                      QCustom2Projection  -> updateQCustom2 (Just f) mr
                      QIndexProjection    -> updateQCustom (Just f) mr
                      QparQperProjection  -> updateQCustom (Just f) mr
                      QxQyQzProjection    -> updateQCustom (Just f) mr
                      RealSpaceProjection -> updateQCustom2 (Just f) mr
                      PixelsProjection    -> updateQCustom2 (Just f) mr
