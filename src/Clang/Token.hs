{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Token
(
-- * Getting information about tokens
  getKind
, getSpelling
, getLocation
, getExtent

-- * Converting to and from lists of tokens
, tokenize
, annotateTokens
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

getKind :: ClangBase m => FFI.Token s' -> ClangT s m FFI.TokenKind
getKind t = liftIO $ FFI.getTokenKind t

getSpelling :: ClangBase m => FFI.TranslationUnit s' -> FFI.Token s'' -> ClangT s m (FFI.ClangString s)
getSpelling = FFI.getTokenSpelling

getLocation :: ClangBase m => FFI.TranslationUnit s' -> FFI.Token s''
            -> ClangT s m (FFI.SourceLocation s)
getLocation tu tk = liftIO $ FFI.getTokenLocation mkProxy tu tk

getExtent :: ClangBase m => FFI.TranslationUnit s' -> FFI.Token s''
          -> ClangT s m (FFI.SourceRange s)
getExtent tu tk = liftIO $ FFI.getTokenExtent mkProxy tu tk

tokenize :: ClangBase m => FFI.TranslationUnit s' -> FFI.SourceRange s''
         -> ClangT s m (FFI.TokenList s)
tokenize = FFI.tokenize

annotateTokens ::
     ClangBase m =>
     FFI.TranslationUnit s' -- ^ The translation unit related to the tokens
  -> FFI.TokenList s'' -- ^ Token list that you want cursors for
  -> ClangT s m (FFI.CursorList s) -- ^ Cursors corresponding to the tokens
annotateTokens = FFI.annotateTokens
