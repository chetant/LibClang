{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.String
( ClangString
, unpack
, unpackByteString
, unsafeUnpackByteString
, unpackText
, hashString
) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

type ClangString = FFI.CXString

unpack :: ClangBase m => ClangString -> ClangT s m String
unpack s = do
  str <- liftIO $ FFI.getString s
  return $! str

unpackByteString :: ClangBase m => ClangString -> ClangT s m B.ByteString
unpackByteString s = do
  str <- liftIO $ FFI.getByteString s
  return $! str

unsafeUnpackByteString :: ClangBase m => ClangString -> ClangT s m B.ByteString
unsafeUnpackByteString s = do
  str <- liftIO $ FFI.unsafeGetByteString s
  return $! str

unpackText :: ClangBase m => ClangString -> ClangT s m T.Text
unpackText s = do
  -- Since unsafeGetByteString does not make a copy, this doesn't actually
  -- require the two copies that it appears to employ.
  str <- liftIO $ FFI.unsafeGetByteString s
  return $! TE.decodeUtf8With TEE.lenientDecode str

hashString :: ClangBase m => ClangString -> ClangT s m Int
hashString s = return $! fromIntegral $ FFI.getStringHash s
