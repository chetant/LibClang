{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

-- | LibClang is a Haskell binding to the libclang library, a compiler
-- front-end for C-family languages. It allows you to produce and walk
-- an AST, get a list of diagnostic warnings and errors, perform code
-- completion, and more.
--
-- Your starting point for using LibClang should be this module, which
-- exports the 'ClangT' monad and the functions you'll need to start
-- analyzing source code. The other modules in this library, such as
-- "Clang.Cursor" and "Clang.Type", are meant to be imported
-- qualified, and provide the functions you'll need to get more
-- detailed information about the AST.
module Clang (
-- * Parsing
  parseSourceFile

-- * Traversing the AST
, FFI.CursorList
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

-- * Traversing inclusions
, FFI.Inclusion(..)
, FFI.InclusionList
, getInclusions

-- * The ClangT monad
, ClangT
, Clang
, ClangBase
, ClangValue(..)
, ClangValueList(..)
, clangScope

-- * Clang AST types
, FFI.Comment (..)
, FFI.CommentKind (..)
, FFI.CommentInlineCommandRenderKind (..)
, FFI.CommentParamPassDirection (..)
, FFI.CompletionString
, FFI.CompletionResult
, FFI.CompletionChunkKind(..)
, FFI.CodeCompleteFlags(..)
, FFI.CodeCompleteResults
, FFI.AvailabilityKind(..)
, FFI.CompletionContext(..)
, FFI.CursorKind(..)
, FFI.LinkageKind(..)
, FFI.LanguageKind(..)
, FFI.Cursor
, FFI.CursorSet
, FFI.ParentedCursor(..)
, FFI.ObjCPropertyAttrKind(..)
, FFI.ObjCDeclQualifierKind(..)
, FFI.NameRefFlags(..)
, FFI.Version(..)
, FFI.PlatformAvailability(..)
, FFI.PlatformAvailabilityInfo(..)
, FFI.DiagnosticSeverity(..)
, FFI.DiagnosticDisplayOptions(..)
, FFI.LoadDiagError(..)
, FFI.Diagnostic
, FFI.DiagnosticSet
, FFI.File
, FFI.Remapping
, FFI.SourceLocation
, FFI.SourceRange
, FFI.ClangString
, FFI.Token
, FFI.TokenKind(..)
, FFI.Index
, FFI.ReparseFlags
, FFI.SaveTranslationUnitFlags(..)
, FFI.TranslationUnit
, FFI.TranslationUnitFlags(..)
, FFI.UnsavedFile
, FFI.GlobalOptFlags(..)
, FFI.globalOpt_ThreadBackgroundPriorityForAll
, FFI.Module (..)
, FFI.Type
, FFI.TypeKind(..)
, FFI.CallingConv(..)
, FFI.CXXAccessSpecifier(..)
, FFI.TypeLayoutError(..)
, FFI.RefQualifierKind(..)
) where

import Control.Monad.Trans (lift)
import qualified Data.Vector as DV

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad
import qualified Clang.TranslationUnit as TU

-- | Parses a source file using libclang and allows you to analyze the
-- resulting AST using a callback.
--
-- More flexible alternatives to 'parseSourceFile' are available in
-- "Clang.TranslationUnit".
parseSourceFile :: ClangBase m
                => FilePath  -- ^ Source filename
                -> [String]  -- ^ Clang-compatible compilation arguments
                -> (forall s. FFI.TranslationUnit s -> ClangT s m a)  -- ^ Callback
                -> m (Maybe a)
parseSourceFile path args f =
  TU.withCreateIndex False False $ \index ->
    TU.withParse index (Just path) args DV.empty [] f

-- | Gets an 'FFI.CursorList' of the children of this 'FFI.Cursor'.
getChildren :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CursorList s)
getChildren = FFI.getChildren

-- | Gets an 'FFI.CursorList' of all descendants of this
-- 'FFI.Cursor'. If you are planning on visiting all the descendants
-- anyway, this is often more efficient than calling 'getChildren'
-- repeatedly. The descendants are listed according to a preorder
-- traversal of the AST.
getDescendants :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CursorList s)
getDescendants = FFI.getDescendants

-- | Like 'getDescendants', but each descendant is annotated with its
-- parent AST node. This provides enough information to replicate the
-- preorder traversal of the AST, but maintains the performance
-- benefits relative to 'getChildren'.
getParentedDescendants :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ParentedCursorList s)
getParentedDescendants = FFI.getParentedDescendants

-- | Gets an 'FFI.CursorList' of all declarations in this 'FFI.TranslationUnit'.
getDeclarations :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.CursorList s)
getDeclarations = FFI.getDeclarations

-- | Like 'getDeclarations', but each declaration is annotated with
-- its parent AST node.
getParentedDeclarations :: ClangBase m => FFI.TranslationUnit s'
                        -> ClangT s m (FFI.ParentedCursorList s)
getParentedDeclarations = FFI.getParentedDeclarations

-- | Gets an 'FFI.CursorList' of all references in this 'FFI.TranslationUnit'.
getReferences :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.CursorList s)
getReferences = FFI.getReferences

-- | Like 'getReferences', but each reference is annotated with
-- its parent AST node.
getParentedReferences :: ClangBase m => FFI.TranslationUnit s'
                      -> ClangT s m (FFI.ParentedCursorList s)
getParentedReferences = FFI.getParentedReferences

-- | Gets two 'FFI.CursorList's, one containing all declarations in
-- this 'FFI.TranslationUnit', and another containing all
-- references. If you need both lists, this is more efficient than
-- calling 'getDeclarations' and 'getReferences' individually, as it
-- only traverses the AST once.
getDeclarationsAndReferences :: ClangBase m => FFI.TranslationUnit s'
                             -> ClangT s m (FFI.CursorList s, FFI.CursorList s)
getDeclarationsAndReferences = FFI.getDeclarationsAndReferences

-- | Like 'getDeclarationsAndReferences', but each reference is annotated with
-- its parent AST node.
getParentedDeclarationsAndReferences :: ClangBase m => FFI.TranslationUnit s'
                                     -> ClangT s m (FFI.ParentedCursorList s,
                                                    FFI.ParentedCursorList s)
getParentedDeclarationsAndReferences = FFI.getParentedDeclarationsAndReferences

-- | Gets all inclusions in this 'FFI.TranslationUnit'.
getInclusions :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.InclusionList s)
getInclusions = FFI.getInclusions

-- | 'Clang' is a specialization of the 'ClangT' monad to 'IO',
-- provided as a convenience. If you have a more complicated monad
-- stack, you may find it useful to define your own 'Clang'-like
-- type synonym.
type Clang s a = ClangT s IO a

-- | Runs a monadic computation with libclang and frees all the
-- resources allocated by that computation immediately.
clangScope :: ClangBase m => (forall s. ClangT s m a) -> ClangT s' m a
clangScope = lift . runClangT
{-# INLINEABLE clangScope #-}
