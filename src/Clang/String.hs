module Clang.String
( ClangString
, unpack
, unpackByteString
, unsafeUnpackByteString
, unpackText
, hash
) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

type ClangString = FFI.CXString

unpack :: ClangString -> ClangApp s String
unpack s = do
  str <- liftIO $ FFI.getString s
  return $! str

unpackByteString :: ClangString -> ClangApp s B.ByteString
unpackByteString s = do
  str <- liftIO $ FFI.getByteString s
  return $! str

unsafeUnpackByteString :: ClangString -> ClangApp s B.ByteString
unsafeUnpackByteString s = do
  str <- liftIO $ FFI.unsafeGetByteString s
  return $! str

unpackText :: ClangString -> ClangApp s T.Text
unpackText s = do
  -- Since unsafeGetByteString does not make a copy, this doesn't actually
  -- require the two copies that it appears to employ.
  str <- liftIO $ FFI.unsafeGetByteString s
  return $! (TE.decodeUtf8With TEE.lenientDecode str)

hash :: ClangString -> ClangApp s Word64
hash s = do
  h <- liftIO $ FFI.getStringHash s
  return $! h
