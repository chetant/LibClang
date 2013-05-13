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

getKind :: FFI.Token -> ClangApp s FFI.TokenKind
getKind t = liftIO $ FFI.getTokenKind t

getSpelling :: FFI.TranslationUnit -> FFI.Token -> ClangApp s FFI.CXString
getSpelling tu tk = FFI.registerCXString $ FFI.getTokenSpelling tu tk

getLocation :: FFI.TranslationUnit -> FFI.Token -> ClangApp s FFI.SourceLocation
getLocation tu tk = liftIO $ FFI.getTokenLocation tu tk

getExtent :: FFI.TranslationUnit -> FFI.Token -> ClangApp s FFI.SourceRange
getExtent tu tk = liftIO $ FFI.getTokenExtent tu tk

tokenize :: FFI.TranslationUnit -> FFI.SourceRange -> ClangApp s [FFI.Token]
tokenize tu sr = liftIO $ FFI.tokenize tu sr
