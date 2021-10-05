{-# LANGUAGE GADTs #-}
{-
    Copyright  : Copyright (C) 2014-2021 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Main where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (LogLevel (LevelDebug), MonadLogger,
                                            filterLogger, runStdoutLoggingT)
import           Data.Attoparsec.Text      (parseOnly)
import           Data.Text                 (pack)
import           Options.Applicative       (CommandFields, Mod, argument,
                                            command, eitherReader, execParser,
                                            fullDesc, header, helper,
                                            hsubparser, info, metavar, optional,
                                            progDesc, str, (<**>))
import           Options.Applicative.Types (Parser)


import           Hkl.Binoculars


data Options = Process (Maybe FilePath) (Maybe ConfigRange)
             | CfgNew (Maybe FilePath)
             | CfgUpdate FilePath
  deriving Show

processOptions :: Parser Options
processOptions = Process
                 <$> optional (argument str (metavar "CONFIG"))
                 <*> optional (argument (eitherReader (parseOnly configRangeP . pack)) (metavar "RANGE"))

processCommand :: Mod CommandFields Options
processCommand = command "process" (info processOptions (progDesc "process data's"))

cfgNewOption :: Parser Options
cfgNewOption = CfgNew <$> optional (argument str (metavar "CONFIG"))

cfgNewCommand :: Mod CommandFields Options
cfgNewCommand = command "cfg-new" (info cfgNewOption (progDesc "new config files"))

cfgUpdateOption :: Parser Options
cfgUpdateOption = CfgUpdate <$> argument str (metavar "CONFIG")

cfgUpdateCommand :: Mod CommandFields Options
cfgUpdateCommand = command "cfg-update" (info cfgUpdateOption (progDesc "update config files"))

options :: Parser Options
options = hsubparser (processCommand <> cfgNewCommand <> cfgUpdateCommand)

run :: (MonadIO m, MonadLogger m, MonadThrow m) => Options -> m ()
run (Process mf mr) = process mf mr
run (CfgNew mf)     = new mf
run (CfgUpdate f)   = update f

main :: IO ()
main = do
  opts' <- execParser opts
  runStdoutLoggingT $ filterLogger (\_ l -> l /=LevelDebug) $ run opts'
    where
      opts = info (options <**> helper)
             ( fullDesc
               <> progDesc "binoculars subcommand"
               <> header "binoculars - bin your data's" )
