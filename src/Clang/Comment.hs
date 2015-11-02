{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for traversing the comment AST and analyzing the nodes it contains.
--
-- To start traversing the comment AST, use 'Clang.Cursor.getParsedComment' to
-- retrieve the comment from an AST node that may be associated with one (for
-- example, any kind of declaration). You can access child nodes in the AST using
-- 'getChildren'. Most of the important information about comment AST nodes is
-- contained in the fields of the 'ParsedComment' type.
--
-- This module is intended to be imported qualified.
module Clang.Comment
(
-- * Navigating the comment AST
  getChildren

-- * Predicates and transformations
, isWhitespace
, hasTrailingNewline
, getTagCommentAsString
, getFullCommentAsHTML
, getFullCommentAsXML

-- * Comment AST nodes
, ParsedComment(..)
, ParamPassDirection(..)
, FFI.ParamPassDirectionKind (..)
, FFI.InlineCommandRenderStyle (..)
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import Clang.Internal.Comment
import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Returns the children nodes of the given comment in the comment AST.
getChildren :: ClangBase m => ParsedComment s' -> ClangT s m [ParsedComment s]
getChildren pc = do
  let c = getFFIComment pc
  numC <- liftIO $ FFI.comment_getNumChildren c
  mayCs <- forM [0..(numC - 1)] $ \i ->
    parseComment =<< liftIO (FFI.comment_getChild mkProxy c i)
  return $ catMaybes mayCs

-- | Returns 'True' if the provided comment is a 'TextComment' which is empty or contains
-- only whitespace, or if it is a 'ParagraphComment' which contains only whitespace
-- 'TextComment' nodes.
isWhitespace :: ClangBase m => ParsedComment s' -> ClangT s m Bool
isWhitespace c = liftIO $ FFI.comment_isWhitespace (getFFIComment c)

-- | Returns 'True' if the provided comment is inline content and has a newline immediately
-- following it in the comment text. Newlines between paragraphs do not count.
hasTrailingNewline :: ClangBase m => ParsedComment s' -> ClangT s m Bool
hasTrailingNewline c = liftIO $ FFI.inlineContentComment_hasTrailingNewline (getFFIComment c)

-- | Returns a string representation of an 'HTMLStartTagComment' or 'HTMLEndTagComment'.
-- For other kinds of comments, returns 'Nothing'.
getTagCommentAsString :: ClangBase m => ParsedComment s' -> ClangT s m (Maybe (FFI.ClangString s))
getTagCommentAsString (HTMLStartTagComment c _ _ _) = Just <$> FFI.hTMLTagComment_getAsString c
getTagCommentAsString (HTMLEndTagComment c _)       = Just <$> FFI.hTMLTagComment_getAsString c
getTagCommentAsString _                             = return Nothing

-- | Converts the given 'FullComment' to an HTML fragment.
--
-- Specific details of HTML layout are subject to change.  Don't try to parse
-- this HTML back into an AST; use other APIs instead.
--
-- Currently the following CSS classes are used:
--
-- * \"para-brief\" for \'brief\' paragraph and equivalent commands;
--
-- * \"para-returns\" for \'returns\' paragraph and equivalent commands;
--
-- * \"word-returns\" for the \"Returns\" word in a \'returns\' paragraph.
--
-- Function argument documentation is rendered as a \<dl\> list with arguments
-- sorted in function prototype order. The following CSS classes are used:
--
-- * \"param-name-index-NUMBER\" for parameter name (\<dt\>);
--
-- * \"param-descr-index-NUMBER\" for parameter description (\<dd\>);
--
-- * \"param-name-index-invalid\" and \"param-descr-index-invalid\" are used if
--   parameter index is invalid.
--
-- Template parameter documentation is rendered as a \<dl\> list with
-- parameters sorted in template parameter list order. The following CSS classes are used:
--
-- * \"tparam-name-index-NUMBER\" for parameter name (\<dt\>);
--
-- * \"tparam-descr-index-NUMBER\" for parameter description (\<dd\>);
--
-- * \"tparam-name-index-other\" and \"tparam-descr-index-other\" are used for
--   names inside template template parameters;
--
-- * \"tparam-name-index-invalid\" and \"tparam-descr-index-invalid\" are used if
--   parameter position is invalid.
getFullCommentAsHTML :: ClangBase m => ParsedComment s' -> ClangT s m (Maybe (FFI.ClangString s))
getFullCommentAsHTML (FullComment c) = Just <$> FFI.fullComment_getAsHTML c
getFullCommentAsHTML _               = return Nothing

-- | Converts the given 'FullComment' to an XML document.
--
-- A Relax NG schema for the XML is distributed in the \"comment-xml-schema.rng\" file
-- inside the libclang source tree.
getFullCommentAsXML :: ClangBase m => ParsedComment s' -> ClangT s m (Maybe (FFI.ClangString s))
getFullCommentAsXML (FullComment c) = Just <$> FFI.fullComment_getAsXML c
getFullCommentAsXML _               = return Nothing
