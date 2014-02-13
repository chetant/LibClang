{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Completion
( FFI.CompletionString
, FFI.CompletionResult
, FFI.CompletionChunkKind
, FFI.CodeCompleteFlags(..)
, FFI.CodeCompleteResults
, FFI.AvailabilityKind
, getChunkKind
, getChunkText
, getChunkCompletionString
, getNumChunks
, getPriority
, getAvailability
, defaultCodeCompleteOptions
, codeCompleteAt
, getDiagnostics
, sortResults
) where

import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Bits ((.&.))
import Data.Maybe (catMaybes)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

getChunkKind :: ClangBase m => FFI.CompletionString s -> Int
             -> ClangT s m FFI.CompletionChunkKind
getChunkKind cs i = liftIO $ FFI.getCompletionChunkKind cs i

getChunkText :: ClangBase m => FFI.CompletionString s -> Int -> ClangT s m (ClangString s)
getChunkText = FFI.getCompletionChunkText

getChunkCompletionString :: ClangBase m => FFI.CompletionString s -> Int ->
                            ClangT s m (FFI.CompletionString s)
getChunkCompletionString cs i = liftIO $ FFI.getCompletionChunkCompletionString cs i

getNumChunks :: ClangBase m => FFI.CompletionString s -> ClangT s m Int
getNumChunks cs = liftIO $ FFI.getNumCompletionChunks cs

getPriority :: ClangBase m => FFI.CompletionString s -> ClangT s m Int
getPriority cs = liftIO $ FFI.getCompletionPriority cs

getAvailability :: ClangBase m => FFI.CompletionString s -> ClangT s m FFI.AvailabilityKind
getAvailability cs = liftIO $ FFI.getCompletionAvailability cs

defaultCodeCompleteOptions :: ClangBase m => ClangT s m [FFI.CodeCompleteFlags]
defaultCodeCompleteOptions = do
  defVal <- liftIO FFI.defaultCodeCompleteOptions
  let val1 = if (defVal .&. 0x01) == 0x01
                then return FFI.CodeComplete_IncludeMacros
                else mzero
  let val2 = if (defVal .&. 0x02) == 0x02
                then return FFI.CodeComplete_IncludeCodePatterns
                else mzero
  return $ catMaybes [val1, val2]
  

codeCompleteAt ::
     ClangBase m =>
     FFI.TranslationUnit s
  -> FilePath -- ^ Filename of the source file
  -> Int -- ^ Line in the source file
  -> Int -- ^ Column on the line
  -> [FFI.UnsavedFile] -- ^ Unsaved files so far
  -> [FFI.CodeCompleteFlags]
  -> ClangT s m (FFI.CodeCompleteResults s)
codeCompleteAt t fname l c ufs opts =
  FFI.codeCompleteAt t fname l c ufs (FFI.getCodeCompleteFlagsSum opts)

sortResults :: ClangBase m => FFI.CodeCompleteResults s -> Int -> ClangT s m ()
sortResults c i = liftIO $ FFI.sortCodeCompletionResults c i

getDiagnostics :: ClangBase m => FFI.CodeCompleteResults s -> ClangT s m [FFI.Diagnostic s]
getDiagnostics c = do
  numD <- liftIO $ FFI.codeCompleteGetNumDiagnostics c
  mapM (FFI.codeCompleteGetDiagnostic c) [0..(numD-1)]

