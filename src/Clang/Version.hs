{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module allows you to query the version of libclang that this
-- library provides bindings for.
module Clang.Version
( getVersion
, apiVersion
, encodedAPIVersion
) where

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Return a version string, suitable for showing to a user, but not
-- intended to be parsed (the format is not guaranteed to be stable).
-- This is the version usually used for libclang packages and public
-- documentation.
getVersion :: ClangBase m => ClangT s m (FFI.ClangString s)
getVersion = FFI.getClangVersion

-- | The API version, in (major, minor) format. This is a semantic
-- version that tracks only the libclang API itself. It's unrelated to
-- the associated LLVM or libclang version.
apiVersion :: (Int, Int)
apiVersion = (FFI.versionMajor, FFI.versionMinor)

-- | The API version, encoded as a single number.
encodedAPIVersion :: Int
encodedAPIVersion = FFI.encodedVersion
