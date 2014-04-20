{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module allows you to query the version of libclang that this
-- library provides bindings for. Libclang actually has two
-- versions. The version string, returned by 'getVersion', reports the
-- version usually used for libclang packages and public
-- documentation. The API version, returned by 'apiVersion, is a
-- semantic version that tracks only the libclang API itself.
module Clang.Version
( getVersion
, apiVersion
, encodedAPIVersion
) where

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Return a version string, suitable for showing to a user, but not
-- intended to be parsed (the format is not guaranteed to be stable).
getVersion :: ClangBase m => ClangT s m (FFI.ClangString s)
getVersion = FFI.getClangVersion

-- | The API version, in (major, minor) format.
apiVersion :: (Int, Int)
apiVersion = (FFI.versionMajor, FFI.versionMinor)

-- | The API version, encoded as a single number.
encodedAPIVersion :: Int
encodedAPIVersion = FFI.encodedVersion
