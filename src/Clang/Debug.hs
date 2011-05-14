module Clang.Debug
(
 enableStackTraces
,getVersion
-- ,toggleCrashRecovery
) where

import System.IO.Unsafe(unsafePerformIO)
import qualified Clang.FFI as FFI

enableStackTraces = FFI.enableStackTraces
getVersion = unsafePerformIO (FFI.getCString =<< FFI.getClangVersion)
-- toggleCrashRecovery = FFI.toggleCrashRecovery
