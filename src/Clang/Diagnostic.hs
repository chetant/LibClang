module Clang.Diagnostic
(
 FFI.DiagnosticSeverity(..)
,FFI.DiagnosticDisplayOptions(..)
,FFI.Diagnostic
) where

import System.IO.Unsafe(unsafePerformIO)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

getDiagnostics :: FFI.TranslationUnit -> [FFI.Diagnostic]
getDiagnostics t = unsafePerformIO $ do
                     numDiags <- FFI.getNumDiagnostics t
                     mapM (FFI.getDiagnostic t) [0..(numDiags-1)]

formatDiagnostic :: [FFI.DiagnosticDisplayOptions] -> FFI.Diagnostic -> IO String
formatDiagnostic [] diag = FFI.getCString =<< FFI.formatDiagnostic diag =<< FFI.defaultDiagnosticDisplayOptions
formatDiagnostic opts diag = FFI.getCString =<< FFI.formatDiagnostic diag (FFI.getDiagnosticDispOptSum opts)

getSeverity = unsafePerformIO . FFI.getDiagnosticSeverity
getSpelling = unsafePerformIO . FFI.getDiagnosticSpelling
getOptions = unsafePerformIO . FFI.getDiagnosticOption
getCategory = unsafePerformIO . FFI.getDiagnosticCategory

getRanges :: FFI.Diagnostic -> [FFI.SourceRange]
getRanges d = unsafePerformIO $ do
                numRanges <- FFI.getDiagnosticNumRanges d
                mapM (FFI.getDiagnosticRange d) [0..(numRanges-1)]
getFixIts :: FFI.Diagnostic -> [(FFI.SourceRange, FFI.CXString)]
getFixIts d = unsafePerformIO $ do
                          numFixes <- FFI.getDiagnosticNumFixIts d
                          mapM (FFI.getDiagnosticFixIt d) [0..(numFixes-1)]

-- Category functions
getCategoryName = unsafePerformIO . FFI.getDiagnosticCategoryName
