{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'FFI.File's, which represent
-- references to files in the libclang AST.
--
-- This module is intended to be imported qualified.
module Clang.File
(
-- * Creating files
  createFromPath

-- * File properties
, getName
, isMultipleIncludeGuarded
, getMTime
, getPosixMTime

-- * Unique IDs
, getUniqueId
, FFI.UniqueId
) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Create a new 'FFI.File' value from the provided path.
createFromPath :: ClangBase m => FFI.TranslationUnit s' -> FilePath -> ClangT s m (FFI.File s)
createFromPath t f = liftIO $ FFI.getFile mkProxy t f

-- | Retrieve the filename of the given file.
getName :: ClangBase m => FFI.File s' -> ClangT s m (FFI.ClangString s)
getName = FFI.getFileName

-- | Determines whether the given file is guarded against multiple
-- inclusions, either with the conventional '#ifdef' / '#define' / '#endif'
-- macro guards or with '#pragma once'.
isMultipleIncludeGuarded :: ClangBase m => FFI.TranslationUnit s' -> FFI.File s''
                         -> ClangT s m Bool
isMultipleIncludeGuarded t f = liftIO $ FFI.isFileMultipleIncludeGuarded t f

-- | Returns the last modification time of the given file, represented
-- as a 'UTCTime'.
getMTime :: ClangBase m => FFI.File s' -> ClangT s m UTCTime
getMTime f = liftIO $ posixSecondsToUTCTime . realToFrac <$> FFI.getFileTime f

-- | Returns the last modification time of the given file, represented
-- as a 'POSIXTime'.
getPosixMTime :: ClangBase m => FFI.File s' -> ClangT s m POSIXTime
getPosixMTime f = liftIO $ realToFrac <$> FFI.getFileTime f

-- | Retrieves a unique ID for the given file. If no unique ID can be
-- generated, returns 'Nothing'.
getUniqueId :: ClangBase m => FFI.File s' -> ClangT s m (Maybe FFI.UniqueId)
getUniqueId f = liftIO $ FFI.getFileUniqueID f
