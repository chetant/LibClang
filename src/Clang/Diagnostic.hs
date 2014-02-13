{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits ((.&.))
import Data.Maybe (catMaybes)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

getDiagnostics :: ClangBase m => FFI.TranslationUnit s -> ClangT s m [FFI.Diagnostic s]
getDiagnostics t = do
  numDiags <- liftIO $ FFI.getNumDiagnostics t
  mapM (FFI.getDiagnostic t) [0..(numDiags-1)]

formatDiagnostic :: ClangBase m => [FFI.DiagnosticDisplayOptions] -> FFI.Diagnostic s
                 -> ClangT s m (ClangString s)
formatDiagnostic [] diag =
  FFI.formatDiagnostic diag =<< liftIO FFI.defaultDiagnosticDisplayOptions
formatDiagnostic opts diag =
  FFI.formatDiagnostic diag (FFI.getDiagnosticDispOptSum opts)

getSeverity :: ClangBase m => FFI.Diagnostic s -> ClangT s m FFI.DiagnosticSeverity
getSeverity d = liftIO $ FFI.getDiagnosticSeverity d

getSpelling :: ClangBase m => FFI.Diagnostic s -> ClangT s m (ClangString s)
getSpelling = FFI.getDiagnosticSpelling

getOptions :: ClangBase m => FFI.Diagnostic s -> ClangT s m (ClangString s, ClangString s)
getOptions = FFI.getDiagnosticOption

defaultDisplayOptions :: ClangBase m => ClangT s m [FFI.DiagnosticDisplayOptions]
defaultDisplayOptions = do
  defVal <- liftIO FFI.defaultDiagnosticDisplayOptions
  let val1 = if (defVal .&. 0x1) == 0x1 then return FFI.Diagnostic_DisplaySourceLocation else mzero
  let val2 = if (defVal .&. 0x2) == 0x2 then return FFI.Diagnostic_DisplayColumn else mzero
  let val3 = if (defVal .&. 0x4) == 0x4 then return FFI.Diagnostic_DisplaySourceRanges else mzero
  let val4 = if (defVal .&. 0x8) == 0x8 then return FFI.Diagnostic_DisplayOption else mzero
  let val5 = if (defVal .&. 0x10) == 0x10 then return FFI.Diagnostic_DisplayCategoryId else mzero
  let val6 = if (defVal .&. 0x20) == 0x20 then return FFI.Diagnostic_DisplayCategoryName else mzero
  return $ catMaybes [val1, val2, val3, val4, val5, val6]


getCategory :: ClangBase m => FFI.Diagnostic s -> ClangT s m Int
getCategory d = liftIO $ FFI.getDiagnosticCategory d

getRanges :: ClangBase m => FFI.Diagnostic s -> ClangT s m [FFI.SourceRange s]
getRanges d = liftIO $ do
                numRanges <- FFI.getDiagnosticNumRanges d
                mapM (FFI.getDiagnosticRange d) [0..(numRanges-1)]

getFixIts :: ClangBase m => FFI.Diagnostic s -> ClangT s m [(FFI.SourceRange s, ClangString s)]
getFixIts d = do
  numFixes <- liftIO $ FFI.getDiagnosticNumFixIts d
  mapM (FFI.getDiagnosticFixIt d) [0..(numFixes - 1)]

-- Category functions

getCategoryName :: ClangBase m => FFI.Diagnostic s -> ClangT s m (ClangString s)
getCategoryName = FFI.getDiagnosticCategoryText
