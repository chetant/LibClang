module Clang.Traversal
( annotateTokens
, getChildren
, FFI.ChildList
, getInclusions
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

getChildren :: FFI.Cursor -> ClangApp s FFI.ChildList
getChildren c = FFI.registerChildList $ FFI.getChildren c

getInclusions :: FFI.TranslationUnit -> ClangApp s FFI.InclusionList
getInclusions tu = FFI.registerInclusionList $ FFI.getInclusions tu
