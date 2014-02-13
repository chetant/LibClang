{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Traversals for the libclang AST.
--
-- For efficiency, traversal functions return a
-- 'Data.Vector.Storable.Vector'. For cases where performance is
-- not a concern, 'toList' can be used to convert these vectors
-- to a standard Haskell list.

module Clang.Traversal (
-- * Basic traversals
  FFI.CursorList
, getChildren
, getDescendants
, FFI.ParentedCursorList
, getParentedDescendants

-- * Inclusion traversals
, FFI.Inclusion(..)
, FFI.InclusionList
, getInclusions

-- * Token traversals
, annotateTokens

-- * Convenience reexports
, toList
) where

import Data.Vector.Storable (toList)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

annotateTokens ::
     ClangBase m =>
     FFI.TranslationUnit s -- ^ The translation unit related to the tokens
  -> [FFI.Token] -- ^ Token list that you want cursors for
  -> ClangT s m FFI.CursorList -- ^ Cursors corresponding to the tokens
annotateTokens tu ts = FFI.registerCursorList $ FFI.annotateTokens tu ts

getChildren :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CursorList
getChildren c = FFI.registerCursorList $ FFI.getChildren c

getDescendants :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CursorList
getDescendants c = FFI.registerCursorList $ FFI.getDescendants c

getParentedDescendants :: ClangBase m => FFI.Cursor -> ClangT s m FFI.ParentedCursorList
getParentedDescendants c = FFI.registerParentedCursorList $ FFI.getParentedDescendants c

getInclusions :: ClangBase m => FFI.TranslationUnit s -> ClangT s m FFI.InclusionList
getInclusions tu = FFI.registerInclusionList $ FFI.getInclusions tu
