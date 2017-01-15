{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for performing code completion.
--
-- To get started with code completion, it's enough to parse a file with
-- 'Clang.parseSourceFile' and pass the 'FFI.TranslationUnit' to 'codeCompleteAt'.
-- This will return a 'FFI.CodeCompleteResults' value, from which you can
-- retrieve a list of completion strings using 'getResults'. Each completion
-- string in turn consists of a series of chunks, which you can retrieve using
-- 'getChunks'.
--
-- This module is intended to be imported qualified.
module Clang.Completion
(
-- * Code completion
  codeCompleteAt
, FFI.CodeCompleteFlags(..)
, FFI.CodeCompleteResults

-- * Completion results
, getResults
, FFI.CompletionString
, sortResults
, getDiagnostics
, getContexts
, FFI.CompletionContext(..)
, getContainerKind
, getContainerUSR
, getObjCSelector

-- * Completion strings
, getChunks
, Chunk(..)
, FFI.ChunkKind(..)
, getPriority
, getAvailability
, getAnnotations
, getParent
, getBriefComment
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import qualified Data.Vector as DV

import Clang.Internal.BitFlags
import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Perform code completion at a given location in a translation unit.
--
-- This function performs code completion at a particular file, line, and
-- column within source code, providing results that suggest potential
-- code snippets based on the context of the completion. The basic model
-- for code completion is that Clang will parse a complete source file,
-- performing syntax checking up to the location where code completion has
-- been requested. At that point, a special code completion token is passed
-- to the parser, which recognizes this token and determines, based on the
-- current location in the C \/ Objective-C \/ C++ grammar and the state of
-- semantic analysis, what completions to provide. These completions are
-- returned via a 'FFI.CodeCompleteResults' value.
--
-- Code completion itself is meant to be triggered by the client when the
-- user types punctuation characters or whitespace, at which point the
-- code completion location will coincide with the cursor. For example, if \'p\'
-- is a pointer, code completion might be triggered after the \'-\' and then
-- after the \'>\' in \'p->\'. When the code completion location is after the \'>\',
-- the completion results will provide, e.g., the members of the struct that
-- \'p\' points to. The client is responsible for placing the cursor at the
-- beginning of the token currently being typed, then filtering the results
-- based on the contents of the token. For example, when code-completing for
-- the expression \'p->get\', the client should provide the location just after
-- the \'>\' (e.g., pointing at the \'g\') to this code completion hook. Then, the
-- client can filter the results based on the current token text (\'get\'), only
-- showing those results that start with \'get\'. The intent of this interface
-- is to separate the relatively high-latency acquisition of code completion
-- results from the filtering of results on a per-character basis, which must
-- have a lower latency.
codeCompleteAt ::
      ClangBase m
   => FFI.TranslationUnit s'  -- ^ The translation unit in which code completion should occur.
                              -- The source files for this translation unit need not be
                              -- completely up-to-date (and the contents of those source files
                              -- may be overridden via the 'FFI.UnsavedFile' vector). Cursors
                              -- referring into the translation unit may be invalidated by
                              -- this invocation.
  -> FilePath  -- ^ The name of the source file where code
               -- completion should be performed. This filename may be any file
               -- included in the translation unit.
  -> Int  -- ^ The line at which code completion should occur.
  -> Int  -- ^ The column at which code completion should occur.
          -- Note that the column should point just after the syntactic construct that
          -- initiated code completion, and not in the middle of a lexical token.
  -> DV.Vector FFI.UnsavedFile  -- ^ Files that have not yet been saved to disk
                                -- but may be required for parsing or code completion, including
                                -- the contents of those files.  The contents and name of these
                                -- files (as specified by 'FFI.UnsavedFile') are copied when
                                -- necessary, so the client only needs to guarantee their
                                -- validity until the call to this function returns.
  -> Maybe [FFI.CodeCompleteFlags]  -- ^ Extra options that control the behavior of code
                                    -- completion, or 'Nothing' to use the default set.
  -> ClangT s m (FFI.CodeCompleteResults s)
codeCompleteAt t fname l c ufs mayOpts = do
  opts <- case mayOpts of
    Just os -> return os
    Nothing -> unFlags <$> liftIO FFI.defaultCodeCompleteOptions
  FFI.codeCompleteAt t fname l c ufs (orFlags opts)

-- | Retrieves a list of code completion results.
--
-- The first element of each tuple is the completion string, which describes how to insert
-- this result into the editing buffer. Use 'getChunks' to analyze it further.
--
-- The second element of each tuple is the kind of entity that this completion refers to.
-- It will be a macro, keyword, or declaration describing the entity that the completion
-- is referring to.
getResults :: ClangBase m => FFI.CodeCompleteResults s'
           -> ClangT s m [(FFI.CompletionString s, FFI.CursorKind)]
getResults rs = do
  numS <- liftIO $ FFI.codeCompleteGetNumResults rs
  forM [0..(numS - 1)] $ \i ->
    FFI.codeCompleteGetResult rs i

-- | Sort the provided code completion results in case-insensitive alphabetical order.
sortResults :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m ()
sortResults c = liftIO $ FFI.sortCodeCompletionResults c

-- | Retrieves the diagnostics associated with the given code completion.
getDiagnostics :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m [FFI.Diagnostic s]
getDiagnostics c = do
  numD <- liftIO $ FFI.codeCompleteGetNumDiagnostics c
  mapM (FFI.codeCompleteGetDiagnostic c) [0..(numD - 1)]

-- | Determines which kinds of completions are appropriate for the context
-- of the given code completion.
getContexts :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m [FFI.CompletionContext]
getContexts rs = unFlags <$> liftIO (FFI.codeCompleteGetContexts rs)

-- | Returns a cursor kind for the container associated with the given code
-- completion. Only contexts like member accesses and Objective-C message sends
-- have containers; for other kinds of contexts, a cursor kind of
-- 'FFI.InvalidCodeCursor' is returned.
--
-- The second element of the result tuple is a boolean indicating whether libclang
-- has incomplete information about the container. A 'True' value indicates that
-- information about the container is incomplete.
getContainerKind :: ClangBase m => FFI.CodeCompleteResults s'
                 -> ClangT s m (FFI.CursorKind, Bool)
getContainerKind rs = liftIO $ FFI.codeCompleteGetContainerKind rs

-- | Given a code completion context, returns a "Clang.USR" for the appropriate
-- container. Only contexts like member accesses and Objective-C message sends
-- have containers; for other kinds of contexts, the empty string is returned.
getContainerUSR :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m (FFI.ClangString s)
getContainerUSR = FFI.codeCompleteGetContainerUSR

-- | Returns the currently-entered selector for an Objective-C message
-- send, formatted like \"initWithFoo:bar:\". This function is only guaranteed
-- to return a non-empty string if the completion context is an Objective-C
-- instance message or class message send, which you can check with 'getContexts'.
getObjCSelector :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m (FFI.ClangString s)
getObjCSelector = FFI.codeCompleteGetObjCSelector

-- | The completion string is a template that describes not only the completion itself,
-- but also information about how it should be presented to the user. It is divided into
-- a list of chunks, with each kind of chunk playing a different role in the template.
data Chunk s
  = TextChunk FFI.ChunkKind (FFI.ClangString s)
  | OptionalChunk (FFI.CompletionString s)
    deriving (Eq, Ord, Typeable)

-- | Retrieves the chunks within a completion string.
--
-- Each \"chunk\" contains either a piece of text with a specific \"kind\"
-- that describes how that text should be interpreted by the client, or
-- another completion string.
getChunks :: ClangBase m => FFI.CompletionString s' -> ClangT s m [Chunk s]
getChunks cs = do
  numC <- liftIO $ FFI.getNumCompletionChunks cs
  forM [0..(numC - 1)] $ \i -> do
    kind <- liftIO $ FFI.getCompletionChunkKind cs i
    case kind of
      FFI.OptionalChunkKind ->
        OptionalChunk <$> (liftIO $ FFI.getCompletionChunkCompletionString cs i)
      _ ->
        TextChunk kind <$> FFI.getCompletionChunkText cs i

-- | Determines the priority of this code completion string.
--
-- The priority of a code completion indicates how likely it is that this
-- particular completion is the completion that the user will select. The
-- priority is selected by various internal heuristics. Smaller values
-- indicate more likely completions.
getPriority :: ClangBase m => FFI.CompletionString s' -> ClangT s m Int
getPriority cs = liftIO $ FFI.getCompletionPriority cs

-- | Determines the availability of the entity that this code completion
-- string refers to.
getAvailability :: ClangBase m => FFI.CompletionString s' -> ClangT s m FFI.AvailabilityKind
getAvailability cs = liftIO $ FFI.getCompletionAvailability cs

-- | Retrieves the annotations associated with the given completion string.
getAnnotations :: ClangBase m => FFI.CompletionString s' -> ClangT s m [FFI.ClangString s]
getAnnotations cs = do
  numA <- liftIO $ FFI.getCompletionNumAnnotations cs
  mapM (FFI.getCompletionAnnotation cs) [0..(numA - 1)]

-- | Retrieves the parent context of the given completion string.
--
-- The parent context of a completion string is the semantic parent of
-- the declaration (if any) that the code completion represents. For example,
-- a code completion for an Objective-C method would have the method's class
-- or protocol as its context. A completion string representing a method on
-- the NSObject class might have a parent context of \"NSObject\".
getParent :: ClangBase m => FFI.CompletionString s' -> ClangT s m (FFI.ClangString s)
getParent = FFI.getCompletionParent

-- | Retrieves the brief documentation comment attached to the declaration
-- that corresponds to the given completion string.
getBriefComment :: ClangBase m => FFI.CompletionString s' -> ClangT s m (FFI.ClangString s)
getBriefComment = FFI.getCompletionBriefComment
