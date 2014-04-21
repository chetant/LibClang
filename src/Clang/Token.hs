{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'Clang.Token's.
--
-- This module is intended to be imported qualified.
module Clang.Token
(
-- * Token lists
  tokenize
, List

-- * Token kinds
, getKind

-- * Mapping between tokens and source code
, getSpelling
, getLocation
, getExtent
, annotateTokens
) where

import Control.Monad.IO.Class
import qualified Data.Vector.Storable as DVS

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Tokenizes the source code described by the given range into raw
-- lexical tokens.
tokenize :: ClangBase m
         => FFI.TranslationUnit s'  -- ^ The translation list
                                    -- containing the source code.
         -> FFI.SourceRange s''     -- ^ The source range in which text
                                    -- should be tokenized.
         -> ClangT s m (List s)
tokenize = FFI.tokenize

-- | A list of tokens, stored as a 'DVS.Vector' for efficiency.
type List s = DVS.Vector (FFI.Token s)

-- | Determines the kind of the given token, expressed as a
-- 'FFI.TokenKind' value.
getKind :: ClangBase m => FFI.Token s' -> ClangT s m FFI.TokenKind
getKind t = liftIO $ FFI.getTokenKind t

-- | Retrieves the \'spelling\', or textual representation, of the
-- given token.
getSpelling :: ClangBase m => FFI.TranslationUnit s' -> FFI.Token s'' -> ClangT s m (FFI.ClangString s)
getSpelling = FFI.getTokenSpelling

-- | Returns the source location of the given token.
getLocation :: ClangBase m => FFI.TranslationUnit s' -> FFI.Token s''
            -> ClangT s m (FFI.SourceLocation s)
getLocation tu tk = liftIO $ FFI.getTokenLocation mkProxy tu tk

-- | Retrieves the source range that covers the given token.
getExtent :: ClangBase m => FFI.TranslationUnit s' -> FFI.Token s''
          -> ClangT s m (FFI.SourceRange s)
getExtent tu tk = liftIO $ FFI.getTokenExtent mkProxy tu tk

-- | Annotates the given set of tokens by providing cursors for each token
-- that can be mapped to a specific entity within the abstract syntax tree.
-- 
-- This is equivalent to invoking 'Clang.Cursor.getCursor' on the
-- source locations of each of these tokens. The cursors provided are
-- filtered, so that only those cursors that have a direct
-- correspondence to the token are accepted. For example, given a
-- function call \'f(x)\', 'Clang.Cursor.getCursor' would provide the
-- following cursors:
-- 
--   * When the cursor is over the \'f\', a 'Clang.DeclRefExpr' cursor
--     referring to \'f\'.
--
--   * When the cursor is over the \'(\' or the \')\', a
--     'Clang.CallExpr' referring to \'f\'.
--
--   * When the cursor is over the \'x\', a 'Clang.DeclRefExpr' cursor
--     referring to \'x\'.
-- 
-- Only the first and last of these cursors will occur within the
-- annotate, since the tokens \'f\' and \'x\' directly refer to a function
-- and a variable, respectively, but the parentheses are just a small
-- part of the full syntax of the function call expression, which is
-- not provided as an annotation.
annotateTokens :: ClangBase m
               => FFI.TranslationUnit s' -- ^ The translation unit that owns the tokens.
               -> List s''               -- ^ The tokens to annotate.
               -> ClangT s m (FFI.CursorList s)
annotateTokens = FFI.annotateTokens
