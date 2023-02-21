{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Utils
    ( hasContent
    , logDebugNSH
    , logErrorNSH
    , withCString
    ) where

import           Control.Monad.Logger  (MonadLogger, logDebugN, logErrorN)
import           Data.Text             (Text, length, pack)
import           Data.Text.Foreign     (unsafeCopyToPtr)
import           Data.Text.IO          (writeFile)
import           Data.Word             (Word8)
import           Foreign.C.String      (CString)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Storable      (pokeByteOff)
import           GHC.Ptr               (castPtr)
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       (takeDirectory)

hasContent ∷ FilePath → Text → IO ()
hasContent f c = do
    createDirectoryIfMissing True (takeDirectory f)
    Data.Text.IO.writeFile f c
    print $ "--> created : " ++ f

logDebugNSH :: (Show a, MonadLogger m) => a -> m ()
logDebugNSH = logDebugN . pack . show

logErrorNSH :: (Show a, MonadLogger m) => a -> m ()
logErrorNSH = logErrorN . pack . show

-- from Text-2.0.1 TO remove later

withCString :: Text -> (CString -> IO a) -> IO a
withCString t action = do
  let len = Data.Text.length t
  allocaBytes (len + 1) $ \buf -> do
    unsafeCopyToPtr t buf
    pokeByteOff buf len (0 :: Word8)
    action (castPtr buf)
