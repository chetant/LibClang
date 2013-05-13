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

import Control.Monad(mzero)
import Control.Monad.IO.Class
import Data.Bits((.&.))
import Data.Maybe(catMaybes)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getChunkKind :: FFI.CompletionString -> Int -> ClangApp s FFI.CompletionChunkKind
getChunkKind cs i = liftIO $ FFI.getCompletionChunkKind cs i

getChunkText :: FFI.CompletionString -> Int -> ClangApp s FFI.CXString
getChunkText cs i = FFI.registerCXString $ FFI.getCompletionChunkText cs i

getChunkCompletionString :: FFI.CompletionString -> Int -> ClangApp s FFI.CompletionString
getChunkCompletionString cs i = liftIO $ FFI.getCompletionChunkCompletionString cs i

getNumChunks :: FFI.CompletionString -> ClangApp s Int
getNumChunks cs = liftIO $ FFI.getNumCompletionChunks cs

getPriority :: FFI.CompletionString -> ClangApp s Int
getPriority cs = liftIO $ FFI.getCompletionPriority cs

getAvailability :: FFI.CompletionString -> ClangApp s FFI.AvailabilityKind
getAvailability cs = liftIO $ FFI.getCompletionAvailability cs

defaultCodeCompleteOptions :: ClangApp s [FFI.CodeCompleteFlags]
defaultCodeCompleteOptions = do
  defVal <- liftIO $ FFI.defaultCodeCompleteOptions
  let val1 = if (defVal .&. 0x01) == 0x01 then return FFI.CodeComplete_IncludeMacros else mzero
  let val2 = if (defVal .&. 0x02) == 0x02 then return FFI.CodeComplete_IncludeCodePatterns else mzero
  return $ catMaybes [val1, val2]
  

codeCompleteAt :: FFI.TranslationUnit
               -> FilePath -- ^ Filename of the source file
               -> Int -- ^ Line in the source file
               -> Int -- ^ Column on the line
               -> [FFI.UnsavedFile] -- ^ Unsaved files so far
               -> [FFI.CodeCompleteFlags]
               -> ClangApp s FFI.CodeCompleteResults
codeCompleteAt t fname l c ufs opts = liftIO $ FFI.codeCompleteAt t fname l c ufs (FFI.getCodeCompleteFlagsSum opts)

sortResults :: FFI.CodeCompleteResults -> Int -> ClangApp s ()
sortResults c i = liftIO $ FFI.sortCodeCompletionResults c i

getDiagnostics :: FFI.CodeCompleteResults -> ClangApp s [FFI.Diagnostic]
getDiagnostics c = do
    numD <- liftIO $ FFI.codeCompleteGetNumDiagnostics c
    mapM getDiag [0..(numD-1)]
  where
    getDiag n = FFI.registerDiagnostic $ FFI.codeCompleteGetDiagnostic c n

