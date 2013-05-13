module Clang.Debug
( enableStackTraces
, getVersion
-- , toggleCrashRecovery
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

enableStackTraces :: ClangApp s ()
enableStackTraces = liftIO $ FFI.enableStackTraces

getVersion :: ClangApp s FFI.CXString
getVersion = FFI.registerCXString $ FFI.getClangVersion

-- toggleCrashRecovery = FFI.toggleCrashRecovery
