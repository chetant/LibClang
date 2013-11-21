module Clang.Traversal
( annotateTokens
, ChildVisitor
, FFI.ChildVisitResult(..)
, visitChildren
, getChildren
, FFI.ChildList
, InclusionVisitor
, getInclusions
, getInclusions'
, FFI.Inclusion(..)
, FFI.InclusionList
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.ClangApp

annotateTokens :: FFI.TranslationUnit -- ^ The translation unit related to the tokens
               -> [FFI.Token] -- ^ Token list that you want cursors for
               -> ClangApp s [FFI.Cursor] -- ^ Cursors corresponding to the tokens
annotateTokens tu ts = liftIO $ FFI.annotateTokens tu ts

type ChildVisitor s = FFI.Cursor -> FFI.Cursor -> ClangApp s FFI.ChildVisitResult

visitChildren :: FFI.Cursor -> ChildVisitor s -> ClangApp s Bool
visitChildren c cv = do
  run <- mkClangAppRunner
  liftIO $ FFI.visitChildren c (\child parent -> run $ cv child parent)

getChildren :: FFI.Cursor -> ClangApp s FFI.ChildList
getChildren c = FFI.registerChildList $ FFI.getChildren c

type InclusionVisitor s = FFI.File -> [FFI.SourceLocation] -> ClangApp s ()

getInclusions :: FFI.TranslationUnit -> InclusionVisitor s -> ClangApp s ()
getInclusions tu iv = do
    run <- mkClangAppRunner
    liftIO $ FFI.getInclusions tu (\f ls -> run $ iv f ls)

getInclusions' :: FFI.TranslationUnit -> ClangApp s FFI.InclusionList
getInclusions' tu = FFI.registerInclusionList $ FFI.getInclusionsX tu
