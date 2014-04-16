{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Completion
( getChunkKind
, getChunkText
, getChunkCompletionString
, getNumChunks
, getPriority
, getAvailability
, getAnnotations
, getParent
, getBriefComment
, getCursorCompletionString
, defaultCodeCompleteOptions
, codeCompleteAt
, sortResults
, getDiagnostics
, getContexts
, getContainerKind
, getContainerUSR
, getObjCSelector
) where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Vector as DV

import Clang.Internal.BitFlags
import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

getChunkKind :: ClangBase m => FFI.CompletionString s' -> Int
             -> ClangT s m FFI.CompletionChunkKind
getChunkKind cs i = liftIO $ FFI.getCompletionChunkKind cs i

getChunkText :: ClangBase m => FFI.CompletionString s' -> Int -> ClangT s m (FFI.ClangString s)
getChunkText = FFI.getCompletionChunkText

getChunkCompletionString :: ClangBase m => FFI.CompletionString s' -> Int ->
                            ClangT s m (FFI.CompletionString s)
getChunkCompletionString cs i = liftIO $ FFI.getCompletionChunkCompletionString cs i

getNumChunks :: ClangBase m => FFI.CompletionString s' -> ClangT s m Int
getNumChunks cs = liftIO $ FFI.getNumCompletionChunks cs

getPriority :: ClangBase m => FFI.CompletionString s' -> ClangT s m Int
getPriority cs = liftIO $ FFI.getCompletionPriority cs

getAvailability :: ClangBase m => FFI.CompletionString s' -> ClangT s m FFI.AvailabilityKind
getAvailability cs = liftIO $ FFI.getCompletionAvailability cs

getAnnotations :: ClangBase m => FFI.CompletionString s' -> ClangT s m [FFI.ClangString s]
getAnnotations cs = do
  numA <- liftIO $ FFI.getCompletionNumAnnotations cs
  mapM (FFI.getCompletionAnnotation cs) [0..(numA - 1)]

getParent :: ClangBase m => FFI.CompletionString s' -> ClangT s m (FFI.ClangString s)
getParent = FFI.getCompletionParent

getBriefComment :: ClangBase m => FFI.CompletionString s' -> ClangT s m (FFI.ClangString s)
getBriefComment = FFI.getCompletionBriefComment

getCursorCompletionString :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CompletionString s)
getCursorCompletionString c = liftIO $ FFI.getCursorCompletionString c

defaultCodeCompleteOptions :: ClangBase m => ClangT s m [FFI.CodeCompleteFlags]
defaultCodeCompleteOptions = unFlags <$> liftIO FFI.defaultCodeCompleteOptions

codeCompleteAt ::
     ClangBase m =>
     FFI.TranslationUnit s'
  -> FilePath -- ^ Filename of the source file
  -> Int -- ^ Line in the source file
  -> Int -- ^ Column on the line
  -> DV.Vector FFI.UnsavedFile -- ^ Unsaved files so far
  -> [FFI.CodeCompleteFlags]
  -> ClangT s m (FFI.CodeCompleteResults s)
codeCompleteAt t fname l c ufs opts = FFI.codeCompleteAt t fname l c ufs (orFlags opts)

sortResults :: ClangBase m => FFI.CodeCompleteResults s' -> Int -> ClangT s m ()
sortResults c i = liftIO $ FFI.sortCodeCompletionResults c i

getDiagnostics :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m [FFI.Diagnostic s]
getDiagnostics c = do
  numD <- liftIO $ FFI.codeCompleteGetNumDiagnostics c
  mapM (FFI.codeCompleteGetDiagnostic c) [0..(numD - 1)]

getContexts :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m [FFI.CompletionContext]
getContexts rs = unFlags <$> liftIO (FFI.codeCompleteGetContexts rs)

getContainerKind :: ClangBase m => FFI.CodeCompleteResults s'
                 -> ClangT s m (FFI.CursorKind, Bool)
getContainerKind rs = liftIO $ FFI.codeCompleteGetContainerKind rs

getContainerUSR :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m (FFI.ClangString s)
getContainerUSR = FFI.codeCompleteGetContainerUSR

getObjCSelector :: ClangBase m => FFI.CodeCompleteResults s' -> ClangT s m (FFI.ClangString s)
getObjCSelector = FFI.codeCompleteGetObjCSelector
