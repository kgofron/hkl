{-# LANGUAGE GADTs #-}
{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
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
import           Control.Monad.Logger      (LogLevel (LevelDebug), LoggingT,
                                            MonadLogger, filterLogger,
                                            runStdoutLoggingT)
import           Data.Attoparsec.Text      (parseOnly)
import           Data.Text                 (pack)
import           Options.Applicative       (CommandFields, Mod, argument,
                                            command, eitherReader, execParser,
                                            fullDesc, header, help, helper,
                                            hsubparser, info, long, metavar,
                                            optional, progDesc, short, str,
                                            switch, (<**>))
import           Options.Applicative.Types (Parser)


import           Hkl.Binoculars


data FullOptions = FullOptions Bool Options
  deriving Show

data Options = Process (Maybe FilePath) (Maybe ConfigRange)
             | CfgNew ProjectionType (Maybe FilePath)
             | CfgUpdate FilePath
  deriving Show

debug :: Parser Bool
debug = switch ( long "debug" <> short 'd' <> help "Print debug informations" )

config :: Parser FilePath
config = argument str (metavar "CONFIG")

processOptions :: Parser Options
processOptions = Process
                 <$> optional config
                 <*> optional (argument (eitherReader (parseOnly fieldParser . pack)) (metavar "RANGE"))

processCommand :: Mod CommandFields Options
processCommand = command "process" (info processOptions (progDesc "process data's"))

cfgNewOption :: Parser Options
cfgNewOption = CfgNew
               <$> argument (eitherReader (parseOnly fieldParser . pack)) (metavar "PROJECTION")
               <*> optional config

cfgNewCommand :: Mod CommandFields Options
cfgNewCommand = command "cfg-new" (info cfgNewOption (progDesc "new config files"))

cfgUpdateOption :: Parser Options
cfgUpdateOption = CfgUpdate <$> config

cfgUpdateCommand :: Mod CommandFields Options
cfgUpdateCommand = command "cfg-update" (info cfgUpdateOption (progDesc "update config files"))

options :: Parser FullOptions
options = FullOptions
          <$> debug
          <*> hsubparser (processCommand <> cfgNewCommand <> cfgUpdateCommand)

run :: (MonadIO m, MonadLogger m, MonadThrow m) => Options -> m ()
run (Process mf mr) = process mf mr
run (CfgNew p mf)   = new p mf
run (CfgUpdate f)   = update f


main :: IO ()
main = do
  (FullOptions d opts') <- execParser opts
  runStdoutLoggingT $ debugLogging d $ run opts'
    where
      debugLogging :: Bool -> LoggingT m a -> LoggingT m a
      debugLogging d = if d then id else filterLogger (\_ l -> l /=LevelDebug)

      opts = info (options <**> helper)
             ( fullDesc
               <> progDesc "binoculars subcommand"
               <> header "binoculars - bin your data's" )
