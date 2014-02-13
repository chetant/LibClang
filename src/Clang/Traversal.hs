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
, FFI.TokenList
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
  -> FFI.TokenList s -- ^ Token list that you want cursors for
  -> ClangT s m (FFI.CursorList s) -- ^ Cursors corresponding to the tokens
annotateTokens tu ts = FFI.registerCursorList $ FFI.annotateTokens tu ts

getChildren :: ClangBase m => FFI.Cursor s -> ClangT s m (FFI.CursorList s)
getChildren c = FFI.registerCursorList $ FFI.getChildren c

getDescendants :: ClangBase m => FFI.Cursor s -> ClangT s m (FFI.CursorList s)
getDescendants c = FFI.registerCursorList $ FFI.getDescendants c

getParentedDescendants :: ClangBase m => FFI.Cursor s -> ClangT s m (FFI.ParentedCursorList s)
getParentedDescendants c = FFI.registerParentedCursorList $ FFI.getParentedDescendants c

getInclusions :: ClangBase m => FFI.TranslationUnit s -> ClangT s m (FFI.InclusionList s)
getInclusions = FFI.getInclusions
