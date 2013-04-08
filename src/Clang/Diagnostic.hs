module Clang.Diagnostic
( FFI.DiagnosticSeverity(..)
, FFI.DiagnosticDisplayOptions(..)
, FFI.Diagnostic

, getDiagnostics
, defaultDisplayOptions
, formatDiagnostic

, getSeverity
, getSpelling
, getOptions
, getCategory

, getRanges
, getFixIts

, getCategoryName
) where

import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Bits ((.&.))
import Data.Maybe (catMaybes)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getDiagnostics :: FFI.TranslationUnit -> ClangApp [FFI.Diagnostic]
getDiagnostics t = liftIO $ do
                     numDiags <- FFI.getNumDiagnostics t
                     mapM (FFI.getDiagnostic t) [0..(numDiags-1)]

formatDiagnostic :: [FFI.DiagnosticDisplayOptions] -> FFI.Diagnostic -> ClangApp String
formatDiagnostic [] diag = liftIO $ FFI.getCString =<< FFI.formatDiagnostic diag =<< FFI.defaultDiagnosticDisplayOptions
formatDiagnostic opts diag = liftIO $ FFI.getCString =<< FFI.formatDiagnostic diag (FFI.getDiagnosticDispOptSum opts)

getSeverity :: FFI.Diagnostic -> ClangApp FFI.DiagnosticSeverity
getSeverity d = liftIO $ FFI.getDiagnosticSeverity d

getSpelling :: FFI.Diagnostic -> ClangApp FFI.CXString
getSpelling d = liftIO $ FFI.getDiagnosticSpelling d

getOptions :: FFI.Diagnostic -> ClangApp (FFI.CXString, FFI.CXString)
getOptions d = liftIO $ FFI.getDiagnosticOption d

defaultDisplayOptions :: ClangApp [FFI.DiagnosticDisplayOptions]
defaultDisplayOptions = do
  defVal <- liftIO $ FFI.defaultDiagnosticDisplayOptions
  let val1 = if (defVal .&. 0x1) == 0x1 then return FFI.Diagnostic_DisplaySourceLocation else mzero
  let val2 = if (defVal .&. 0x2) == 0x2 then return FFI.Diagnostic_DisplayColumn else mzero
  let val3 = if (defVal .&. 0x4) == 0x4 then return FFI.Diagnostic_DisplaySourceRanges else mzero
  let val4 = if (defVal .&. 0x8) == 0x8 then return FFI.Diagnostic_DisplayOption else mzero
  let val5 = if (defVal .&. 0x10) == 0x10 then return FFI.Diagnostic_DisplayCategoryId else mzero
  let val6 = if (defVal .&. 0x20) == 0x20 then return FFI.Diagnostic_DisplayCategoryName else mzero
  return $ catMaybes [val1, val2, val3, val4, val5, val6]


getCategory :: FFI.Diagnostic -> ClangApp Int
getCategory d = liftIO $ FFI.getDiagnosticCategory d

getRanges :: FFI.Diagnostic -> ClangApp [FFI.SourceRange]
getRanges d = liftIO $ do
                numRanges <- FFI.getDiagnosticNumRanges d
                mapM (FFI.getDiagnosticRange d) [0..(numRanges-1)]

getFixIts :: FFI.Diagnostic -> ClangApp [(FFI.SourceRange, FFI.CXString)]
getFixIts d = liftIO $ do
                numFixes <- FFI.getDiagnosticNumFixIts d
                mapM (FFI.getDiagnosticFixIt d) [0..(numFixes-1)]

-- Category functions

getCategoryName :: FFI.Diagnostic -> ClangApp FFI.CXString
getCategoryName d = liftIO $ FFI.getDiagnosticCategoryText d
