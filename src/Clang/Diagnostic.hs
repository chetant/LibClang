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

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits ((.&.))
import Data.Maybe (catMaybes)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getDiagnostics :: FFI.TranslationUnit -> ClangApp s [FFI.Diagnostic]
getDiagnostics t = do
    numDiags <- liftIO $ FFI.getNumDiagnostics t
    mapM getDiag [0..(numDiags-1)]
  where
    getDiag n = FFI.registerDiagnostic $ FFI.getDiagnostic t n

formatDiagnostic :: [FFI.DiagnosticDisplayOptions] -> FFI.Diagnostic -> ClangApp s FFI.CXString
formatDiagnostic [] diag =
  FFI.registerCXString $ FFI.formatDiagnostic diag
                     =<< FFI.defaultDiagnosticDisplayOptions
formatDiagnostic opts diag =
  FFI.registerCXString $ FFI.formatDiagnostic diag (FFI.getDiagnosticDispOptSum opts)

getSeverity :: FFI.Diagnostic -> ClangApp s FFI.DiagnosticSeverity
getSeverity d = liftIO $ FFI.getDiagnosticSeverity d

getSpelling :: FFI.Diagnostic -> ClangApp s FFI.CXString
getSpelling d = FFI.registerCXString $ FFI.getDiagnosticSpelling d

getOptions :: FFI.Diagnostic -> ClangApp s (FFI.CXString, FFI.CXString)
getOptions d = do
  (a, b) <- liftIO $ FFI.getDiagnosticOption d
  (,) <$> FFI.registerCXString (pure a) <*> FFI.registerCXString (pure b)

defaultDisplayOptions :: ClangApp s [FFI.DiagnosticDisplayOptions]
defaultDisplayOptions = do
  defVal <- liftIO $ FFI.defaultDiagnosticDisplayOptions
  let val1 = if (defVal .&. 0x1) == 0x1 then return FFI.Diagnostic_DisplaySourceLocation else mzero
  let val2 = if (defVal .&. 0x2) == 0x2 then return FFI.Diagnostic_DisplayColumn else mzero
  let val3 = if (defVal .&. 0x4) == 0x4 then return FFI.Diagnostic_DisplaySourceRanges else mzero
  let val4 = if (defVal .&. 0x8) == 0x8 then return FFI.Diagnostic_DisplayOption else mzero
  let val5 = if (defVal .&. 0x10) == 0x10 then return FFI.Diagnostic_DisplayCategoryId else mzero
  let val6 = if (defVal .&. 0x20) == 0x20 then return FFI.Diagnostic_DisplayCategoryName else mzero
  return $ catMaybes [val1, val2, val3, val4, val5, val6]


getCategory :: FFI.Diagnostic -> ClangApp s Int
getCategory d = liftIO $ FFI.getDiagnosticCategory d

getRanges :: FFI.Diagnostic -> ClangApp s [FFI.SourceRange]
getRanges d = liftIO $ do
                numRanges <- FFI.getDiagnosticNumRanges d
                mapM (FFI.getDiagnosticRange d) [0..(numRanges-1)]

getFixIts :: FFI.Diagnostic -> ClangApp s [(FFI.SourceRange, FFI.CXString)]
getFixIts d = do
    numFixes <- liftIO $ FFI.getDiagnosticNumFixIts d
    forM [0..(numFixes - 1)] $ \i -> do
      (r, s) <- liftIO $ FFI.getDiagnosticFixIt d i
      (,) <$> pure r <*> FFI.registerCXString (pure s)

-- Category functions

getCategoryName :: FFI.Diagnostic -> ClangApp s FFI.CXString
getCategoryName d = FFI.registerCXString $ FFI.getDiagnosticCategoryText d
