{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Utils
    ( hasContent
    , logDebugNSH
    , logErrorNSH
    ) where

import           Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import           Data.Text            (Text, pack)
import           Data.Text.IO         (writeFile)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)

hasContent ∷ FilePath → Text → IO ()
hasContent f c = do
    createDirectoryIfMissing True (takeDirectory f)
    Data.Text.IO.writeFile f c
    print $ "--> created : " ++ f

logDebugNSH :: (Show a, MonadLogger m) => a -> m ()
logDebugNSH = logDebugN . pack . show

logErrorNSH :: (Show a, MonadLogger m) => a -> m ()
logErrorNSH = logErrorN . pack . show
