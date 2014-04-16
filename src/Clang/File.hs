{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.File
( getName
, getPOSIXTime
, getUTCTime
, getFile
, getFileUniqueId
, isFileMultipleIncludeGuarded
) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

getName :: ClangBase m => FFI.File s' -> ClangT s m (FFI.ClangString s)
getName = FFI.getFileName

getPOSIXTime :: ClangBase m => FFI.File s' -> ClangT s m POSIXTime
getPOSIXTime f = liftIO $ realToFrac <$> FFI.getFileTime f

getUTCTime :: ClangBase m => FFI.File s' -> ClangT s m UTCTime
getUTCTime f = liftIO $ posixSecondsToUTCTime . realToFrac <$> FFI.getFileTime f

getFile :: ClangBase m => FFI.TranslationUnit s' -> FilePath -> ClangT s m (FFI.File s)
getFile t f = liftIO $ FFI.getFile mkProxy t f

getFileUniqueId :: ClangBase m => FFI.File s' -> ClangT s m (Maybe FFI.FileUniqueId)
getFileUniqueId f = liftIO $ FFI.getFileUniqueID f

isFileMultipleIncludeGuarded :: ClangBase m => FFI.TranslationUnit s' -> FFI.File s''
                             -> ClangT s m Bool
isFileMultipleIncludeGuarded t f = liftIO $ FFI.isFileMultipleIncludeGuarded t f
