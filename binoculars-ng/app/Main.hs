{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
    Copyright  : Copyright (C) 2014-2024 Synchrotron SOLEIL
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
import           Control.Monad.Reader      (MonadReader, ReaderT, ask,
                                            runReaderT)
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
             | CfgUpdate FilePath (Maybe ConfigRange)
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
cfgUpdateOption = CfgUpdate
                  <$> config
                  <*> optional (argument (eitherReader (parseOnly fieldParser. pack)) (metavar "RANGE"))

cfgUpdateCommand :: Mod CommandFields Options
cfgUpdateCommand = command "cfg-update" (info cfgUpdateOption (progDesc "update config files"))

options :: Parser FullOptions
options = FullOptions
          <$> debug
          <*> hsubparser (processCommand <> cfgNewCommand <> cfgUpdateCommand)

newtype App a =
    App { unApp :: ReaderT FullOptions (LoggingT IO) a }
    deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader FullOptions
    , MonadLogger
    , MonadThrow
    )

runApp :: FullOptions -> (LoggingT IO a -> IO a) -> App a -> IO a
runApp env runLogging action =
  runLogging $ runReaderT (unApp action) env

app :: App ()
app = do
  (FullOptions _ o) <- ask
  case o of
    (Process mf mr)  -> process mf mr
    (CfgNew p mf)    -> new p mf
    (CfgUpdate f mr) -> update f mr

main :: IO ()
main = do
  let opts = info (options <**> helper)
             ( fullDesc
             <> progDesc "binoculars subcommand"
             <> header "binoculars - bin your data's" )
  fopts@(FullOptions d _) <- execParser opts
  let debugLogging = if d then id else filterLogger (\_ l -> l /=LevelDebug)
  let runLogging = runStdoutLoggingT . debugLogging
  runApp fopts runLogging app
