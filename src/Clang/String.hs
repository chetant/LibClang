module Clang.String
( FFI.CXString
, unpack
, hash
) where

import Control.Monad.IO.Class
import Data.Word

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

unpack :: FFI.CXString -> ClangApp String
unpack s = do
  str <- liftIO $ FFI.getCString s
  return $! str

hash :: FFI.CXString -> ClangApp Word64
hash s = do
  h <- liftIO $ FFI.getCXStringHash s
  return $! h
