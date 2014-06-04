{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'FFI.Module's. An 'FFI.Module' value
-- may be obtained using 'Clang.Cursor.getModule'.
--
-- This module is intended to be imported qualified.
module Clang.Module
( getASTFile
, getParent
, getName
, getFullName
, getTopLevelHeaders
) where

import Control.Monad
import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Retrieves the 'FFI.File' which contains the provided module.
getASTFile :: ClangBase m => FFI.Module s' -> ClangT s m (FFI.File s)
getASTFile m = liftIO $ FFI.module_getASTFile mkProxy m

-- | Given an module, returns either
--
--   * the parent module (for example, for \'std.vector\' the \'std\'
--     module will be returned), or
--
--   * 'Nothing' if the module is top-level.
getParent :: ClangBase m => FFI.Module s' -> ClangT s m (Maybe (FFI.Module s))
getParent m = liftIO $ FFI.module_getParent mkProxy m

-- | Retrieves the name of a module. For example, for \'std.vector\',
-- 'getName' will return \'vector\'.
getName :: ClangBase m => FFI.Module s' -> ClangT s m (FFI.ClangString s)
getName = FFI.module_getName

-- | Retrieves the full name of a module, e.g. \'std.vector\'.
getFullName :: ClangBase m => FFI.Module s' -> ClangT s m (FFI.ClangString s)
getFullName = FFI.module_getFullName

-- | Returns the list of top-level headers associated with the given module.
getTopLevelHeaders :: ClangBase m => FFI.TranslationUnit s' -> FFI.Module s''
                   -> ClangT s m [FFI.File s]
getTopLevelHeaders tu m = do
  numHeaders <- liftIO $ FFI.module_getNumTopLevelHeaders tu m
  forM [0..(numHeaders - 1)] $ \idx ->
    liftIO $ FFI.module_getTopLevelHeader mkProxy tu m idx
