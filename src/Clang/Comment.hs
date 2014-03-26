{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Comment
( FFI.Comment (..)
, FFI.CommentKind (..)
, FFI.CommentInlineCommandRenderKind (..)
, FFI.CommentParamPassDirection (..)
, getKind
, getNumChildren
, getChild
, isWhitespace
, inlineContentComment_hasTrailingNewline
, textComment_getText
, inlineCommandComment_getCommandName
, inlineCommandComment_getRenderKind 
, inlineCommandComment_getArgs 
, htmlTagComment_getTagName
, htmlStartTagComment_isSelfClosing
, htmlStartTag_getAttrs
, blockCommandComment_getCommandName
, blockCommandComment_getArgs
, blockCommandComment_getParagraph
, paramCommandComment_getParamName
, paramCommandComment_isParamIndexValid
, paramCommandComment_getParamIndex
, paramCommandComment_isDirectionExplicit
, paramCommandComment_getDirection
, tParamCommandComment_getParamName
, tParamCommandComment_isParamPositionValid
, tParamCommandComment_getDepth
, tParamCommandComment_getIndex
, verbatimBlockLineComment_getText
, verbatimLineComment_getText
, htmlTagComment_getAsString
, fullComment_getAsHTML
, fullComment_getAsXML 
) where

import Control.Monad
import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

getKind :: ClangBase m => FFI.Comment s' -> ClangT s m FFI.CommentKind
getKind c = liftIO $ FFI.comment_getKind c

getNumChildren :: ClangBase m => FFI.Comment s' -> ClangT s m Int
getNumChildren c = liftIO $ FFI.comment_getNumChildren c

getChild :: ClangBase m => FFI.Comment s' -> Int -> ClangT s m (FFI.Comment s)
getChild c i = liftIO $ FFI.comment_getChild mkProxy c i

isWhitespace :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
isWhitespace c = liftIO $ FFI.comment_isWhitespace c

inlineContentComment_hasTrailingNewline :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
inlineContentComment_hasTrailingNewline c = liftIO $ FFI.inlineContentComment_hasTrailingNewline c

textComment_getText :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
textComment_getText = FFI.textComment_getText

inlineCommandComment_getCommandName :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
inlineCommandComment_getCommandName = FFI.inlineCommandComment_getCommandName

inlineCommandComment_getRenderKind :: ClangBase m => FFI.Comment s'
                                   -> ClangT s m FFI.CommentInlineCommandRenderKind
inlineCommandComment_getRenderKind c = liftIO $ FFI.inlineCommandComment_getRenderKind c

inlineCommandComment_getArgs :: ClangBase m => FFI.Comment s' -> ClangT s m [ClangString s]
inlineCommandComment_getArgs c = do
  numArgs <- liftIO $ FFI.inlineCommandComment_getNumArgs c
  mapM (FFI.inlineCommandComment_getArgText c) [0..(numArgs - 1)]

htmlTagComment_getTagName :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
htmlTagComment_getTagName = FFI.hTMLTagComment_getTagName

htmlStartTagComment_isSelfClosing :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
htmlStartTagComment_isSelfClosing c = liftIO $ FFI.hTMLStartTagComment_isSelfClosing c

htmlStartTag_getAttrs :: ClangBase m => FFI.Comment s' -> ClangT s m [(ClangString s, ClangString s)]
htmlStartTag_getAttrs c = do
  numAttrs <- liftIO $ FFI.hTMLStartTag_getNumAttrs c
  forM [0..numAttrs] $ \attr -> do
    attrName <- FFI.hTMLStartTag_getAttrName c attr
    attrValue <- FFI.hTMLStartTag_getAttrValue c attr
    return (attrName, attrValue)
  
blockCommandComment_getCommandName :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
blockCommandComment_getCommandName = FFI.blockCommandComment_getCommandName

blockCommandComment_getArgs :: ClangBase m => FFI.Comment s' -> ClangT s m [ClangString s]
blockCommandComment_getArgs c = do
  numArgs <- liftIO $ FFI.blockCommandComment_getNumArgs c
  mapM (FFI.blockCommandComment_getArgText c) [0..(numArgs - 1)]

blockCommandComment_getParagraph :: ClangBase m => FFI.Comment s' -> ClangT s m (FFI.Comment s)
blockCommandComment_getParagraph c = liftIO $ FFI.blockCommandComment_getParagraph mkProxy c

paramCommandComment_getParamName :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
paramCommandComment_getParamName = FFI.paramCommandComment_getParamName

paramCommandComment_isParamIndexValid :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
paramCommandComment_isParamIndexValid c = liftIO $ FFI.paramCommandComment_isParamIndexValid c

paramCommandComment_getParamIndex :: ClangBase m => FFI.Comment s' -> ClangT s m Int
paramCommandComment_getParamIndex c = liftIO $ FFI.paramCommandComment_getParamIndex c

paramCommandComment_isDirectionExplicit :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
paramCommandComment_isDirectionExplicit c = liftIO $ FFI.paramCommandComment_isDirectionExplicit c

paramCommandComment_getDirection :: ClangBase m => FFI.Comment s' -> ClangT s m FFI.CommentParamPassDirection
paramCommandComment_getDirection c = liftIO $ FFI.paramCommandComment_getDirection c

tParamCommandComment_getParamName :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
tParamCommandComment_getParamName = FFI.tParamCommandComment_getParamName

tParamCommandComment_isParamPositionValid :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
tParamCommandComment_isParamPositionValid c = liftIO $ FFI.tParamCommandComment_isParamPositionValid c

tParamCommandComment_getDepth :: ClangBase m => FFI.Comment s' -> ClangT s m Int
tParamCommandComment_getDepth c = liftIO $ FFI.tParamCommandComment_getDepth  c

tParamCommandComment_getIndex :: ClangBase m => FFI.Comment s' -> Int -> ClangT s m Int
tParamCommandComment_getIndex c depth = liftIO $ FFI.tParamCommandComment_getIndex c depth

verbatimBlockLineComment_getText :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
verbatimBlockLineComment_getText = FFI.verbatimBlockLineComment_getText

verbatimLineComment_getText :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
verbatimLineComment_getText = FFI.verbatimLineComment_getText

htmlTagComment_getAsString :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
htmlTagComment_getAsString = FFI.hTMLTagComment_getAsString

fullComment_getAsHTML :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
fullComment_getAsHTML = FFI.fullComment_getAsHTML

fullComment_getAsXML :: ClangBase m => FFI.Comment s' -> ClangT s m (ClangString s)
fullComment_getAsXML = FFI.fullComment_getAsXML
