module Clang.String
( FFI.CXString
, unpack
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

unpack :: FFI.CXString -> ClangApp String
unpack s = liftIO $ FFI.getCString s
