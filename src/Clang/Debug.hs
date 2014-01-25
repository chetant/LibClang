{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Debug
( enableStackTraces
, getVersion
-- , toggleCrashRecovery
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

enableStackTraces :: ClangBase m => ClangT s m ()
enableStackTraces = liftIO $ FFI.enableStackTraces

getVersion :: ClangBase m => ClangT s m FFI.CXString
getVersion = FFI.registerCXString $ FFI.getClangVersion

-- toggleCrashRecovery = FFI.toggleCrashRecovery
