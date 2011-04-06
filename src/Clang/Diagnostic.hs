module Clang.Diagnostic
(
 FFI.DiagnosticSeverity(..)
,FFI.DiagnosticDisplayOptions(..)
,FFI.Diagnostic

,getDiagnostics
,getDefaultDisplayOptions
,formatDiagnostic

,getSeverity
,getSpelling
,getOptions
,getCategory

,getRanges
,getFixIts

,getCategoryName
) where

import System.IO.Unsafe(unsafePerformIO)
import Data.Bits((.&.))
import Data.Maybe(catMaybes)
import Control.Monad(mzero)

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

getDefaultDisplayOptions = catMaybes [val1 defVal, val2 defVal, val3 defVal, val4 defVal, val5 defVal, val6 defVal]
    where defVal = unsafePerformIO FFI.defaultDiagnosticDisplayOptions
          val1 v = if (v .&. 0x1) == 0x1 then return FFI.Diagnostic_DisplaySourceLocation else mzero
          val2 v = if (v .&. 0x2) == 0x2 then return FFI.Diagnostic_DisplayColumn else mzero
          val3 v = if (v .&. 0x4) == 0x4 then return FFI.Diagnostic_DisplaySourceRanges else mzero
          val4 v = if (v .&. 0x8) == 0x8 then return FFI.Diagnostic_DisplayOption else mzero
          val5 v = if (v .&. 0x10) == 0x10 then return FFI.Diagnostic_DisplayCategoryId else mzero
          val6 v = if (v .&. 0x20) == 0x20 then return FFI.Diagnostic_DisplayCategoryName else mzero

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
