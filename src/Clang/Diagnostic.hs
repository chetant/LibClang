{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Diagnostic
( FFI.DiagnosticSeverity(..)
, FFI.DiagnosticDisplayOptions(..)
, FFI.LoadDiagError(..)
, FFI.Diagnostic
, FFI.DiagnosticSet

, getDiagnostics
, getChildDiagnostics
, defaultDisplayOptions
, formatDiagnostic

, loadDiagnostics
, getDiagnosticSetFromTU
, getDiagnosticsInSet

, getSeverity
, getSpelling
, getOptions
, getCategory

, getRanges
, getFixIts

, getCategoryName
) where

import Control.Applicative
import Control.Monad.IO.Class

import Clang.Internal.BitFlags
import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

getDiagnostics :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m [FFI.Diagnostic s]
getDiagnostics t = do
  numDiags <- liftIO $ FFI.getNumDiagnostics t
  mapM (FFI.getDiagnostic t) [0..(numDiags - 1)]

getChildDiagnostics :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (FFI.DiagnosticSet s)
getChildDiagnostics = FFI.getChildDiagnostics

formatDiagnostic :: ClangBase m => Maybe [FFI.DiagnosticDisplayOptions] -> FFI.Diagnostic s'
                 -> ClangT s m (ClangString s)
formatDiagnostic Nothing diag     = FFI.formatDiagnostic diag =<<
                                    liftIO FFI.defaultDiagnosticDisplayOptions
formatDiagnostic (Just opts) diag = FFI.formatDiagnostic diag (orFlags opts)

loadDiagnostics :: ClangBase m => FilePath
                -> ClangT s m (Either (FFI.LoadDiagError, ClangString s) (FFI.DiagnosticSet s))
loadDiagnostics = FFI.loadDiagnostics

getDiagnosticSetFromTU :: ClangBase m => FFI.TranslationUnit s'
                       -> ClangT s m (FFI.DiagnosticSet s)
getDiagnosticSetFromTU = FFI.getDiagnosticSetFromTU

getDiagnosticsInSet :: ClangBase m => FFI.DiagnosticSet s' -> ClangT s m [FFI.Diagnostic s]
getDiagnosticsInSet ds = do
  numDiags <- liftIO $ FFI.getNumDiagnosticsInSet ds
  mapM (FFI.getDiagnosticInSet ds) [0..(numDiags - 1)]

getSeverity :: ClangBase m => FFI.Diagnostic s' -> ClangT s m FFI.DiagnosticSeverity
getSeverity d = liftIO $ FFI.getDiagnosticSeverity d

getSpelling :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (ClangString s)
getSpelling = FFI.getDiagnosticSpelling

getOptions :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (ClangString s, ClangString s)
getOptions = FFI.getDiagnosticOption

defaultDisplayOptions :: ClangBase m => ClangT s m [FFI.DiagnosticDisplayOptions]
defaultDisplayOptions = unFlags <$> liftIO FFI.defaultDiagnosticDisplayOptions

getCategory :: ClangBase m => FFI.Diagnostic s' -> ClangT s m Int
getCategory d = liftIO $ FFI.getDiagnosticCategory d

getRanges :: ClangBase m => FFI.Diagnostic s' -> ClangT s m [FFI.SourceRange s]
getRanges d = liftIO $ do
                numRanges <- FFI.getDiagnosticNumRanges d
                mapM (FFI.getDiagnosticRange d) [0..(numRanges - 1)]

getFixIts :: ClangBase m => FFI.Diagnostic s' -> ClangT s m [(FFI.SourceRange s, ClangString s)]
getFixIts d = do
  numFixes <- liftIO $ FFI.getDiagnosticNumFixIts d
  mapM (FFI.getDiagnosticFixIt d) [0..(numFixes - 1)]

-- Category functions

getCategoryName :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (ClangString s)
getCategoryName = FFI.getDiagnosticCategoryText
