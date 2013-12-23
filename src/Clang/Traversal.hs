{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Traversal
( annotateTokens
, getChildren
, getDescendants
, FFI.ChildList
, getParentedDescendants
, FFI.ParentedCursorList
, getInclusions
, FFI.Inclusion(..)
, FFI.InclusionList
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

annotateTokens ::
     ClangBase m =>
     FFI.TranslationUnit -- ^ The translation unit related to the tokens
  -> [FFI.Token] -- ^ Token list that you want cursors for
  -> ClangT s m [FFI.Cursor] -- ^ Cursors corresponding to the tokens
annotateTokens tu ts = liftIO $ FFI.annotateTokens tu ts

getChildren :: ClangBase m => FFI.Cursor -> ClangT s m FFI.ChildList
getChildren c = FFI.registerChildList $ FFI.getChildren c

getDescendants :: ClangBase m => FFI.Cursor -> ClangT s m FFI.ChildList
getDescendants c = FFI.registerChildList $ FFI.getDescendants c

getParentedDescendants :: ClangBase m => FFI.Cursor -> ClangT s m FFI.ParentedCursorList
getParentedDescendants c = FFI.registerParentedCursorList $ FFI.getParentedDescendants c

getInclusions :: ClangBase m => FFI.TranslationUnit -> ClangT s m FFI.InclusionList
getInclusions tu = FFI.registerInclusionList $ FFI.getInclusions tu
