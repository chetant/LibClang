module Clang.Traversal
( annotateTokens
, ChildVisitor
, FFI.ChildVisitResult(..)
, visitChildren
, InclusionVisitor
, getInclusions
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.ClangApp

annotateTokens :: FFI.TranslationUnit -- ^ The translation unit related to the tokens
               -> [FFI.Token] -- ^ Token list that you want cursors for
               -> ClangApp [FFI.Cursor] -- ^ Cursors corresponding to the tokens
annotateTokens tu ts = liftIO $ FFI.annotateTokens tu ts

type ChildVisitor = FFI.Cursor -> FFI.Cursor -> ClangApp FFI.ChildVisitResult

visitChildren :: FFI.Cursor -> ChildVisitor -> ClangApp Bool
visitChildren c cv = do
  run <- mkClangAppRunner
  liftIO $ FFI.visitChildren c (\child parent -> run $ cv child parent)

type InclusionVisitor = FFI.File -> [FFI.SourceLocation] -> ClangApp ()

getInclusions :: FFI.TranslationUnit -> InclusionVisitor -> ClangApp ()
getInclusions tu iv = do
    run <- mkClangAppRunner
    liftIO $ FFI.getInclusions tu (\f ls -> run $ iv f ls)
