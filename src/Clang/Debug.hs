module Clang.Debug
( enableStackTraces
, getVersion
-- , toggleCrashRecovery
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

enableStackTraces :: ClangApp ()
enableStackTraces = liftIO $ FFI.enableStackTraces

getVersion :: ClangApp FFI.CXString
getVersion = FFI.registerCXString $ FFI.getClangVersion

-- toggleCrashRecovery = FFI.toggleCrashRecovery
