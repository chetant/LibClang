module Clang.Traversal
(
 annotateTokens
,FFI.ChildVisitor
,FFI.visitChildren
) where

import qualified Clang.FFI as FFI

annotateTokens :: FFI.TranslationUnit -- ^ The translation unit related to the tokens
               -> [FFI.Token] -- ^ Token list that you want cursors for
               -> IO [FFI.Cursor] -- ^ Cursors corresponding to the tokens
annotateTokens = FFI.annotateTokens