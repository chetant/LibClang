{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module contains debug-related functionality.
module Clang.Debug
( setCrashRecoveryEnabled
, enableStackTraces
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Enable or disable crash recovery.
setCrashRecoveryEnabled :: ClangBase m
                        => Bool -- ^ Whether crash recovery should be enabled.
                        -> ClangT s m ()
setCrashRecoveryEnabled enable = liftIO $ FFI.toggleCrashRecovery enable

-- | Enable stack traces when crashes occur.
enableStackTraces :: ClangBase m => ClangT s m ()
enableStackTraces = liftIO FFI.enableStackTraces
