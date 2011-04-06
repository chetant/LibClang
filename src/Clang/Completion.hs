module Clang.Completion
(
 FFI.CompletionString
,FFI.CompletionResult
,FFI.CompletionChunkKind
,FFI.CodeCompleteFlags(..)
,FFI.CodeCompleteResults
,getChunkKind
,getChunkText
,getChunkCompletionString
,getNumChunks
,getPriority
,getAvailability
,defaultCodeCompleteOptions
,codeCompleteAt
) where

import System.IO.Unsafe(unsafePerformIO)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

getChunkKind cs i = unsafePerformIO (FFI.getCompletionChunkKind cs i)
getChunkText cs i = unsafePerformIO (FFI.getCompletionChunkText cs i)
getChunkCompletionString cs i = unsafePerformIO (FFI.getCompletionChunkCompletionString cs i)
getNumChunks = unsafePerformIO . FFI.getNumCompletionChunks
getPriority = unsafePerformIO . FFI.getCompletionPriority
getAvailability = unsafePerformIO . FFI.getCompletionAvailability

defaultCodeCompleteOptions = unsafePerformIO FFI.defaultCodeCompleteOptions

codeCompleteAt :: FFI.TranslationUnit
               -> FilePath -- ^ Filename of the source file
               -> Int -- ^ Line in the source file
               -> Int -- ^ Column on the line
               -> [FFI.UnsavedFile] -- ^ Unsaved files so far
               -> [FFI.CodeCompleteFlags]
               -> IO FFI.CodeCompleteResults
codeCompleteAt t fname l c ufs opts = FFI.codeCompleteAt t fname l c ufs (FFI.getCodeCompleteFlagsSum opts)
sortResults = FFI.sortCodeCompletionResults
getDiagnostics c = unsafePerformIO $ do
                     numD <- FFI.codeCompleteGetNumDiagnostics c
                     mapM (FFI.codeCompleteGetDiagnostic c) [0..(numD-1)]
