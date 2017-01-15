{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Internal.Comment
( ParsedComment(..)
, ParamPassDirection(..)
, parseComment
, getFFIComment
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | A node in the comment AST.
--
-- Every constructor contains an opaque 'FFI.Comment' value, which represents the AST node
-- itself. The other fields are the user-visible metadata for that node type.
data ParsedComment s
  -- | Plain text. Inline content.
  = TextComment (FFI.Comment s) (FFI.ClangString s)

  -- | A command with word-like arguments that is considered inline content.
  | InlineCommandComment (FFI.Comment s) (FFI.ClangString s) FFI.InlineCommandRenderStyle [FFI.ClangString s]

  -- | An HTML start tag with attributes (represented as name/value pairs). Considered inline content.
  -- The final argument of the constructor is 'True' if the tag is self-closing.
  | HTMLStartTagComment (FFI.Comment s) (FFI.ClangString s) [(FFI.ClangString s, FFI.ClangString s)] Bool

  -- | An HTML end tag. Considered inline content.
  | HTMLEndTagComment (FFI.Comment s) (FFI.ClangString s)

  -- | A paragraph, which contains inline content. The paragraph itself is block content.
  | ParagraphComment (FFI.Comment s)

  -- | A command which has zero or more word-like arguments and a paragraph argument. Block content.
  -- The paragraph argument (of type 'ParsedComment') is also a child of the 'BlockCommandComment'.
  --
  -- As an example, a \'\\brief\' comment creates a 'BlockCommandComment' AST node with no word-like
  -- arguments and a paragraph argument.
  | BlockCommandComment (FFI.Comment s) (FFI.ClangString s) [FFI.ClangString s] (Maybe (ParsedComment s))

  -- | A \'\\param\' or \'\\arg\' command that describes a function parameter. The parameter name,
  -- index in the parameter list (or 'Nothing' if the index is invalid), and parameter passing
  -- direction are provided as constructor fields. The description is provided as a child node.
  | ParamCommandComment (FFI.Comment s) (FFI.ClangString s) (Maybe Int) ParamPassDirection


  -- | A \'\\tparam\' command that describes a template parameter. The parameter name and position
  -- are provided as constructor fields, while the description is provided as a child node.
  --
  -- Since template parameters can be nested, the position is a list with a left-to-right position
  -- at each nesting depth. For example, for the following declaration:
  --
  -- > template<typename C, template<typename T> class TT>
  -- > void test(TT<int> aaa);
  --
  -- The resulting position would be [0] for \'C\', [1] for \'TT\', and [1, 0] for \'T\'.
  | TypeParamCommandComment (FFI.Comment s) (FFI.ClangString s) (Maybe [Int])

  -- | A verbatim block command (e.g. preformatted code). A verbatim block has an opening
  -- and a closing command and contains multiple lines of text, represented as
  -- 'VerbatimBlockLineComment' nodes.
  | VerbatimBlockCommandComment (FFI.Comment s) (Maybe (ParsedComment s))

  -- | A line of text that is contained within a 'VerbatimBlockCommandComment' node.
  | VerbatimBlockLineComment (FFI.Comment s) (FFI.ClangString s)

  -- | A verbatim line command. A verbatim line has an opening command, a single line
  -- of text (up to the newline after the opening command), and has no closing command.
  | VerbatimLineComment (FFI.Comment s) (FFI.ClangString s)

  -- | A full comment attached to a declaration. Contains block content.
  | FullComment (FFI.Comment s)
    deriving (Eq, Ord, Typeable)

-- | A parameter passing direction, either explicitly provided in the comment text or
-- inferred.
data ParamPassDirection
  = ExplicitParamPassDirection FFI.ParamPassDirectionKind
  | InferredParamPassDirection FFI.ParamPassDirectionKind
    deriving (Eq, Ord, Read, Show, Typeable)

parseComment :: ClangBase m => FFI.Comment s -> ClangT s m (Maybe (ParsedComment s))
parseComment c = do
  kind <- liftIO $ FFI.comment_getKind c
  case kind of
    FFI.NullComment                 -> pure Nothing
    FFI.TextComment                 -> Just . TextComment c <$> FFI.textComment_getText c
    FFI.InlineCommandComment        -> do v <- InlineCommandComment c <$> FFI.inlineCommandComment_getCommandName c
                                                                     <*> inlineCommandComment_getRenderKind c
                                                                     <*> inlineCommandComment_getArgs c
                                          return $ Just v
    FFI.HTMLStartTagComment         -> do v <- HTMLStartTagComment c <$> FFI.hTMLTagComment_getTagName c
                                                                     <*> htmlStartTagComment_getAttrs c
                                                                     <*> htmlStartTagComment_isSelfClosing c
                                          return $ Just v
    FFI.HTMLEndTagComment           -> Just . HTMLEndTagComment c <$> FFI.hTMLTagComment_getTagName c
    FFI.ParagraphComment            -> pure $ Just (ParagraphComment c)
    FFI.BlockCommandComment         -> do v <- BlockCommandComment c <$> FFI.blockCommandComment_getCommandName c
                                                                     <*> blockCommandComment_getArgs c
                                                                     <*> blockCommandComment_getParagraph c
                                          return $ Just v
    FFI.ParamCommandComment         -> do v <- ParamCommandComment c <$> FFI.paramCommandComment_getParamName c
                                                                     <*> paramCommandComment_getParamIndex c
                                                                     <*> paramCommandComment_getDirection c
                                          return $ Just v
    FFI.TParamCommandComment        -> do v <- TypeParamCommandComment c
                                                 <$> FFI.tParamCommandComment_getParamName c
                                                 <*> tParamCommandComment_getPosition c
                                          return $ Just v
    FFI.VerbatimBlockCommandComment -> Just . VerbatimBlockCommandComment c <$> blockCommandComment_getParagraph c
    FFI.VerbatimBlockLineComment    -> Just . VerbatimBlockLineComment c <$> FFI.verbatimBlockLineComment_getText c
    FFI.VerbatimLineComment         -> Just . VerbatimLineComment c <$> FFI.verbatimLineComment_getText c
    FFI.FullComment                 -> pure $ Just (FullComment c)

getFFIComment :: ParsedComment s ->  FFI.Comment s
getFFIComment (TextComment c _) = c
getFFIComment (InlineCommandComment c _ _ _) = c
getFFIComment (HTMLStartTagComment c _ _ _) = c
getFFIComment (HTMLEndTagComment c _) = c
getFFIComment (ParagraphComment c) = c
getFFIComment (BlockCommandComment c _ _ _) = c
getFFIComment (ParamCommandComment c _ _ _) = c
getFFIComment (TypeParamCommandComment c _ _) = c
getFFIComment (VerbatimBlockCommandComment c _) = c
getFFIComment (VerbatimBlockLineComment c _) = c
getFFIComment (VerbatimLineComment c _) = c
getFFIComment (FullComment c) = c

inlineCommandComment_getRenderKind :: ClangBase m => FFI.Comment s'
                                   -> ClangT s m FFI.InlineCommandRenderStyle
inlineCommandComment_getRenderKind c = liftIO $ FFI.inlineCommandComment_getRenderKind c

inlineCommandComment_getArgs :: ClangBase m => FFI.Comment s' -> ClangT s m [FFI.ClangString s]
inlineCommandComment_getArgs c = do
  numArgs <- liftIO $ FFI.inlineCommandComment_getNumArgs c
  mapM (FFI.inlineCommandComment_getArgText c) [0..(numArgs - 1)]

htmlStartTagComment_getAttrs :: ClangBase m => FFI.Comment s'
                             -> ClangT s m [(FFI.ClangString s, FFI.ClangString s)]
htmlStartTagComment_getAttrs c = do
  numAttrs <- liftIO $ FFI.hTMLStartTag_getNumAttrs c
  forM [0..numAttrs] $ \attr -> do
    attrName <- FFI.hTMLStartTag_getAttrName c attr
    attrValue <- FFI.hTMLStartTag_getAttrValue c attr
    return (attrName, attrValue)

htmlStartTagComment_isSelfClosing :: ClangBase m => FFI.Comment s' -> ClangT s m Bool
htmlStartTagComment_isSelfClosing c = liftIO $ FFI.hTMLStartTagComment_isSelfClosing c

blockCommandComment_getArgs :: ClangBase m => FFI.Comment s' -> ClangT s m [FFI.ClangString s]
blockCommandComment_getArgs c = do
  numArgs <- liftIO $ FFI.blockCommandComment_getNumArgs c
  mapM (FFI.blockCommandComment_getArgText c) [0..(numArgs - 1)]

blockCommandComment_getParagraph :: ClangBase m => FFI.Comment s' -> ClangT s m (Maybe (ParsedComment s))
blockCommandComment_getParagraph c =
  parseComment =<< liftIO (FFI.blockCommandComment_getParagraph mkProxy c)

paramCommandComment_getParamIndex :: ClangBase m => FFI.Comment s' -> ClangT s m (Maybe Int)
paramCommandComment_getParamIndex c = do
  valid <- liftIO $ FFI.paramCommandComment_isParamIndexValid c
  if valid
    then Just <$> (liftIO $ FFI.paramCommandComment_getParamIndex c)
    else return Nothing

paramCommandComment_getDirection :: ClangBase m => FFI.Comment s' -> ClangT s m ParamPassDirection
paramCommandComment_getDirection c = do
  dir <- liftIO $ FFI.paramCommandComment_getDirection c
  explicit <- liftIO $ FFI.paramCommandComment_isDirectionExplicit c
  return $ if explicit
             then ExplicitParamPassDirection dir
             else InferredParamPassDirection dir

tParamCommandComment_getPosition :: ClangBase m => FFI.Comment s' -> ClangT s m (Maybe [Int])
tParamCommandComment_getPosition c = do
  valid <- liftIO $ FFI.tParamCommandComment_isParamPositionValid c
  if valid
     then do depth <- liftIO $ FFI.tParamCommandComment_getDepth c
             indices <- forM [0..depth] $ \d ->
                          liftIO $ FFI.tParamCommandComment_getIndex c d
             return $ Just indices
     else return Nothing
