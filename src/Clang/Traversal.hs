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
, getDeclarations
, getReferences
, getDeclarationsAndReferences

, FFI.ParentedCursorList
, getParentedDescendants
, getParentedDeclarations
, getParentedReferences
, getParentedDeclarationsAndReferences

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
     FFI.TranslationUnit s' -- ^ The translation unit related to the tokens
  -> FFI.TokenList s'' -- ^ Token list that you want cursors for
  -> ClangT s m (FFI.CursorList s) -- ^ Cursors corresponding to the tokens
annotateTokens = FFI.annotateTokens

getChildren :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CursorList s)
getChildren = FFI.getChildren

getDescendants :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CursorList s)
getDescendants = FFI.getDescendants

getDeclarations :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.CursorList s)
getDeclarations = FFI.getDeclarations

getReferences :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.CursorList s)
getReferences = FFI.getReferences

getDeclarationsAndReferences :: ClangBase m => FFI.TranslationUnit s'
                             -> ClangT s m (FFI.CursorList s, FFI.CursorList s)
getDeclarationsAndReferences = FFI.getDeclarationsAndReferences

getParentedDescendants :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ParentedCursorList s)
getParentedDescendants = FFI.getParentedDescendants

getParentedDeclarations :: ClangBase m => FFI.TranslationUnit s'
                        -> ClangT s m (FFI.ParentedCursorList s)
getParentedDeclarations = FFI.getParentedDeclarations

getParentedReferences :: ClangBase m => FFI.TranslationUnit s'
                      -> ClangT s m (FFI.ParentedCursorList s)
getParentedReferences = FFI.getParentedReferences

getParentedDeclarationsAndReferences :: ClangBase m => FFI.TranslationUnit s'
                                     -> ClangT s m (FFI.ParentedCursorList s,
                                                    FFI.ParentedCursorList s)
getParentedDeclarationsAndReferences = FFI.getParentedDeclarationsAndReferences

getInclusions :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.InclusionList s)
getInclusions = FFI.getInclusions
