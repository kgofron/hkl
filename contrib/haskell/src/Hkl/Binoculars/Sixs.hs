{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
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

import           Control.Monad.Catch                 (MonadThrow)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Logger                (MonadLogger, logDebug,
                                                      logDebugSH, logErrorSH)
import           Path.IO                             (getCurrentDir)
import           Path.Posix                          (parseAbsDir)

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections.Angles
import           Hkl.Binoculars.Projections.Hkl
import           Hkl.Binoculars.Projections.QIndex
import           Hkl.Binoculars.Projections.QparQper
import           Hkl.Binoculars.Projections.QxQyQz

process :: (MonadLogger m, MonadThrow m, MonadIO m)
        => Maybe FilePath -> Maybe (ConfigRange) -> m ()
process mf mr = do
  epreconf <- liftIO $ getPreConfig mf
  $(logDebug) "pre-config red from the config file"
  $(logDebugSH) epreconf
  case epreconf of
    Left e        -> $(logErrorSH) e
    Right preconf -> case (_binocularsPreConfigProjectionType preconf) of
                      AnglesProjection   -> processAngles mf mr
                      Angles2Projection  -> processAngles mf mr
                      HklProjection      -> processHkl mf mr
                      QIndexProjection   -> processQIndex mf mr
                      QparQperProjection -> processQparQper mf mr
                      QxQyQzProjection   -> processQxQyQz mf mr

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
    QIndexProjection   -> newQIndex cwd
    QparQperProjection -> newQparQper cwd
    QxQyQzProjection   -> newQxQyQz cwd

update :: (MonadIO m, MonadLogger m, MonadThrow m) => FilePath -> m ()
update f = do
  epreconf <- liftIO $ getPreConfig (Just f)
  $(logDebug) "pre-config red from the config file"
  $(logDebugSH) epreconf
  case epreconf of
    Left e        -> $(logErrorSH) e
    Right preconf -> case (_binocularsPreConfigProjectionType preconf) of
                      AnglesProjection   -> updateAngles (Just f)
                      Angles2Projection  -> updateAngles (Just f)
                      HklProjection      -> updateHkl (Just f)
                      QIndexProjection   -> updateQIndex (Just f)
                      QparQperProjection -> updateQparQper (Just f)
                      QxQyQzProjection   -> updateQxQyQz (Just f)
