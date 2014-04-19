{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module contains debug-related functionality.
module Clang.Debug
( getVersion
, version
, encodedVersion
, setCrashRecoveryEnabled
, enableStackTraces
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Return a version string, suitable for showing to a user, but not
-- intended to be parsed (the format is not guaranteed to be stable).
getVersion :: ClangBase m => ClangT s m (FFI.ClangString s)
getVersion = FFI.getClangVersion

-- | The API version, in (major, minor) format.
version :: (Int, Int)
version = (FFI.versionMajor, FFI.versionMinor)

-- | The API version, encoded as a single number.
encodedVersion :: Int
encodedVersion = FFI.encodedVersion

-- | Enable or disable crash recovery.
setCrashRecoveryEnabled :: ClangBase m
                        => Bool -- ^ Whether crash recovery should be enabled.
                        -> ClangT s m ()
setCrashRecoveryEnabled enable = liftIO $ FFI.toggleCrashRecovery enable

-- | Enable stack traces when crashes occur.
enableStackTraces :: ClangBase m => ClangT s m ()
enableStackTraces = liftIO FFI.enableStackTraces
