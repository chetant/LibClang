{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

getASTFile :: ClangBase m => FFI.Module s' -> ClangT s m (FFI.File s)
getASTFile m = liftIO $ FFI.module_getASTFile mkProxy m

getParent :: ClangBase m => FFI.Module s' -> ClangT s m (Maybe (FFI.Module s))
getParent m = liftIO $ FFI.module_getParent mkProxy m

getName :: ClangBase m => FFI.Module s' -> ClangT s m (FFI.ClangString s)
getName = FFI.module_getName

getFullName :: ClangBase m => FFI.Module s' -> ClangT s m (FFI.ClangString s)
getFullName = FFI.module_getFullName

getTopLevelHeaders :: ClangBase m => FFI.TranslationUnit s' -> FFI.Module s''
                   -> ClangT s m [FFI.File s]
getTopLevelHeaders tu m = do
  numHeaders <- liftIO $ FFI.module_getNumTopLevelHeaders tu m
  forM [0..numHeaders] $ \idx ->
    liftIO $ FFI.module_getTopLevelHeader mkProxy tu m idx
