{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.File
( FFI.UnsavedFile
, FFI.File
, getName
, hashFile
, getPOSIXTime
, getUTCTime
, getFile
) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getName :: ClangBase m => FFI.File -> ClangT s m FFI.CXString
getName f = FFI.registerCXString $ FFI.getFileName f

hashFile :: ClangBase m => FFI.File -> ClangT s m Int
hashFile f = return $! fromIntegral $ FFI.getFileHash f

getPOSIXTime :: ClangBase m => FFI.File -> ClangT s m POSIXTime
getPOSIXTime f = liftIO $ realToFrac <$> FFI.getFileTime f

getUTCTime :: ClangBase m => FFI.File -> ClangT s m UTCTime
getUTCTime f = liftIO $ posixSecondsToUTCTime . realToFrac <$> FFI.getFileTime f

getFile :: ClangBase m => FFI.TranslationUnit -> FilePath -> ClangT s m FFI.File
getFile t f = liftIO $ FFI.getFile t f
