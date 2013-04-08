module Clang.Debug
( enableStackTraces
, getVersion
-- , toggleCrashRecovery
) where

import qualified Clang.Internal.FFI as FFI

enableStackTraces :: IO ()
enableStackTraces = FFI.enableStackTraces

getVersion :: IO String
getVersion = FFI.getCString =<< FFI.getClangVersion

-- toggleCrashRecovery = FFI.toggleCrashRecovery
