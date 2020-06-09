{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}
{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Main where

import           Options.Applicative       (CommandFields, Mod, argument,
                                            command, execParser, fullDesc,
                                            header, helper, hsubparser, info,
                                            metavar, optional, progDesc, str,
                                            (<**>))
import           Options.Applicative.Types (Parser)


import           Hkl.Binoculars


data Options = Process (Maybe FilePath)
             | New (Maybe FilePath)
  deriving Show

processOptions :: Parser Options
processOptions = Process <$> optional (argument str (metavar "CONFIG"))

processCommand :: Mod CommandFields Options
processCommand = command "process" (info processOptions (progDesc "process data's"))

newOption :: Parser Options
newOption = New <$> optional (argument str (metavar "CONFIG"))

newCommand :: Mod CommandFields Options
newCommand = command "new" (info newOption (progDesc "new config files"))

options :: Parser Options
options = hsubparser (processCommand <> newCommand)

run :: Options -> IO ()
run (Process mf) = process mf
run (New mf)     = new mf

main :: IO ()
main = do
  opts' <- execParser opts
  run opts'
    where
      opts = info (options <**> helper)
             ( fullDesc
               <> progDesc "binoculars subcommand"
               <> header "binoculars - bin your data's" )
