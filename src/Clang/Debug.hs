{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Debug
( enableStackTraces
, getVersion
, toggleCrashRecovery
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

enableStackTraces :: ClangBase m => ClangT s m ()
enableStackTraces = liftIO FFI.enableStackTraces

getVersion :: ClangBase m => ClangT s m (FFI.ClangString s)
getVersion = FFI.getClangVersion

toggleCrashRecovery :: ClangBase m => Bool -> ClangT s m ()
toggleCrashRecovery enable = liftIO $ FFI.toggleCrashRecovery enable
