module Clang.Traversal
( annotateTokens
, FFI.ChildVisitor
, FFI.ChildVisitResult(..)
, FFI.visitChildren
, FFI.InclusionVisitor
, FFI.getInclusions
) where

import qualified Clang.Internal.FFI as FFI

annotateTokens :: FFI.TranslationUnit -- ^ The translation unit related to the tokens
               -> [FFI.Token] -- ^ Token list that you want cursors for
               -> IO [FFI.Cursor] -- ^ Cursors corresponding to the tokens
annotateTokens = FFI.annotateTokens
