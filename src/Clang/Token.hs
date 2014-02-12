{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Token
( FFI.Token
, FFI.TokenKind
, getKind
, getSpelling
, getLocation
, getExtent
, tokenize
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

getKind :: ClangBase m => FFI.Token -> ClangT s m FFI.TokenKind
getKind t = liftIO $ FFI.getTokenKind t

getSpelling :: ClangBase m => FFI.TranslationUnit -> FFI.Token -> ClangT s m (ClangString s)
getSpelling = FFI.getTokenSpelling

getLocation :: ClangBase m => FFI.TranslationUnit -> FFI.Token -> ClangT s m FFI.SourceLocation
getLocation tu tk = liftIO $ FFI.getTokenLocation tu tk

getExtent :: ClangBase m => FFI.TranslationUnit -> FFI.Token -> ClangT s m FFI.SourceRange
getExtent tu tk = liftIO $ FFI.getTokenExtent tu tk

tokenize :: ClangBase m => FFI.TranslationUnit -> FFI.SourceRange -> ClangT s m [FFI.Token]
tokenize tu sr = liftIO $ FFI.tokenize tu sr
