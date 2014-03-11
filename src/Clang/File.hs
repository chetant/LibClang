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
import Clang.String (ClangString)

getName :: ClangBase m => FFI.File s' -> ClangT s m (ClangString s)
getName = FFI.getFileName

hashFile :: ClangBase m => FFI.File s' -> ClangT s m Int
hashFile f = return $! fromIntegral $ FFI.getFileHash f

getPOSIXTime :: ClangBase m => FFI.File s' -> ClangT s m POSIXTime
getPOSIXTime f = liftIO $ realToFrac <$> FFI.getFileTime f

getUTCTime :: ClangBase m => FFI.File s' -> ClangT s m UTCTime
getUTCTime f = liftIO $ posixSecondsToUTCTime . realToFrac <$> FFI.getFileTime f

getFile :: ClangBase m => FFI.TranslationUnit s' -> FilePath -> ClangT s m (FFI.File s)
getFile t f = liftIO $ FFI.getFile mkProxy t f
