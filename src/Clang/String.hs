{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.String
( unpack
, unpackByteString
, unsafeUnpackByteString
, unpackText
, hashString
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

unpack :: ClangBase m => FFI.ClangString s' -> ClangT s m String
unpack = FFI.getString

unpackByteString :: ClangBase m => FFI.ClangString s' -> ClangT s m B.ByteString
unpackByteString = FFI.getByteString

unsafeUnpackByteString :: ClangBase m => FFI.ClangString s' -> ClangT s m B.ByteString
unsafeUnpackByteString = FFI.unsafeGetByteString

unpackText :: ClangBase m => FFI.ClangString s' -> ClangT s m T.Text
unpackText s = do
  -- Since unsafeGetByteString does not make a copy, this doesn't actually
  -- require the two copies that it appears to employ.
  str <- FFI.unsafeGetByteString s
  return $! TE.decodeUtf8With TEE.lenientDecode str

hashString :: ClangBase m => FFI.ClangString s' -> ClangT s m Int
hashString s = return $! fromIntegral $ FFI.getStringHash s
