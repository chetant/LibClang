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
import Clang.String (ClangString)

enableStackTraces :: ClangBase m => ClangT s m ()
enableStackTraces = liftIO FFI.enableStackTraces

getVersion :: ClangBase m => ClangT s m (ClangString s)
getVersion = FFI.getClangVersion

-- toggleCrashRecovery = FFI.toggleCrashRecovery
