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

import           Options.Applicative       (CommandFields, Mod, auto, command,
                                            execParser, fullDesc, header,
                                            helper, hsubparser, info, long,
                                            option, optional, progDesc,
                                            showDefault, (<**>))
import           Options.Applicative.Types (Parser)


import           Hkl.Binoculars


data Options = Process !(Maybe FilePath)

processOptions :: Parser Options
processOptions = Process <$> (optional $ option auto (long "config" <> showDefault))

processCommand :: Mod CommandFields Options
processCommand = command "process" (info processOptions (progDesc "process data's"))

options :: Parser Options
options = hsubparser processCommand

run :: Options -> IO ()
run (Process _) = process

main :: IO ()
main = do
  opts' <- execParser opts
  run opts'
    where
      opts = info (options <**> helper)
             ( fullDesc
               <> progDesc "binoculars subcommand"
               <> header "binoculars - bin your data's" )
