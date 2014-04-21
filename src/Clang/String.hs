{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module contains functions for working with
-- 'FFI.ClangString's, which represent strings that are part of the
-- libclang AST or managed by its API.
module Clang.String (
-- * Conversions
  unpack
, unpackText
, unpackByteString

-- * Unsafe conversions
, unsafeUnpackByteString
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Converts an 'FFI.ClangString' to a 'String'.
unpack :: ClangBase m => FFI.ClangString s' -> ClangT s m String
unpack = FFI.getString

-- | Converts an 'FFI.ClangString' to a 'T.Text'.
unpackText :: ClangBase m => FFI.ClangString s' -> ClangT s m T.Text
unpackText s = do
  -- Since unsafeGetByteString does not make a copy, this doesn't actually
  -- require the two copies that it appears to employ.
  str <- FFI.unsafeGetByteString s
  return $! TE.decodeUtf8With TEE.lenientDecode str

-- | Converts an 'FFI.ClangString' to a 'B.ByteString'. This is faster
-- than 'unpack' and 'unpackText' since it requires no encoding.
unpackByteString :: ClangBase m => FFI.ClangString s' -> ClangT s m B.ByteString
unpackByteString = FFI.getByteString

-- | Creates a 'B.ByteString' that shares the underlying memory of the
-- 'FFI.ClangString'. This is very fast since no copying has to be
-- done. However, it's also unsafe because the 'B.ByteString' may outlive
-- the scope of the 'FFI.ClangString', leading to corruption of
-- its contents or crashes. It's the caller's responsibility to
-- prevent this.
unsafeUnpackByteString :: ClangBase m => FFI.ClangString s' -> ClangT s m B.ByteString
unsafeUnpackByteString = FFI.unsafeGetByteString
