{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'FFI.Diagnostic's, which represent
-- diagnostics - warnings, errors, and the like - produced by libclang.
--
-- 'FFI.Diagnostic's are grouped into 'FFI.DiagnosticSet's. You
-- usually obtain a 'FFI.DiagnosticSet' from a 'FFI.TranslationUnit'
-- using 'Clang.TranslationUnit.getDiagnosticSet', and then analyze it
-- using the functions in this module.
--
-- This module is intended to be imported qualified.
module Clang.Diagnostic
(
-- * Diagnostic sets
  getElements

-- * Diagnostics
, getChildren
, getSeverity
, FFI.Severity(..)
, getSpelling
, getOptions
, getCategoryName
, getCategoryId
, getRanges
, getFixIts

-- * Rendering diagnostics
, format
, FFI.DisplayOptions(..)

-- * Deserializing diagnostics
, load
, FFI.LoadError(..)
) where

import Control.Monad.IO.Class

import Clang.Internal.BitFlags
import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad


-- | Retrieve the diagnostics contained in the given 'FFI.DiagnosticSet'.
--
-- You can obtain a 'FFI.DiagnosticSet' from a 'FFI.TranslationUnit'
-- using 'Clang.TranslationUnit.getDiagnosticSet', or by deserializing
-- it from a file using 'load'.
getElements :: ClangBase m => FFI.DiagnosticSet s' -> ClangT s m [FFI.Diagnostic s]
getElements ds = do
  numDiags <- liftIO $ FFI.getNumDiagnosticsInSet ds
  mapM (FFI.getDiagnosticInSet ds) [0..(numDiags - 1)]

-- | Get the child diagnostics of the given diagnostic.
getChildren :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (FFI.DiagnosticSet s)
getChildren = FFI.getChildDiagnostics

-- | Determine the severity of the given diagnostic.
getSeverity :: ClangBase m => FFI.Diagnostic s' -> ClangT s m FFI.Severity
getSeverity d = liftIO $ FFI.getDiagnosticSeverity d

-- | Retrieve the text of the given diagnostic.
getSpelling :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (FFI.ClangString s)
getSpelling = FFI.getDiagnosticSpelling

-- | Retrieve the name of the command-line option that enabled this diagnostic.
getOptions :: ClangBase m
           => FFI.Diagnostic s'  -- ^ The diagnostic in question.
           -> ClangT s m (FFI.ClangString s, FFI.ClangString s)  -- ^ The options controlling
                                                                 --   this diagnostic. The
                                                                 --   first member of the pair
                                                                 --   is the option to enable
                                                                 --   this diagnostic. The
                                                                 --   second member is the
                                                                 --   option to disable it.
getOptions = FFI.getDiagnosticOption

-- | Get the name of the diagnostic category for the given diagnostic.
--
-- Diagnostics are categorized into groups along with other,
-- related diagnostics (e.g., diagnostics under the same warning
-- flag).
getCategoryName :: ClangBase m => FFI.Diagnostic s' -> ClangT s m (FFI.ClangString s)
getCategoryName = FFI.getDiagnosticCategoryText

-- | Retrieve the category id for this diagnostic.
--
-- For display to the user, use 'getCategoryName'.
getCategoryId :: ClangBase m => FFI.Diagnostic s' -> ClangT s m Int
getCategoryId d = liftIO $ FFI.getDiagnosticCategory d

-- | Retrieve the source ranges associated with this diagnostic.
--
-- A diagnostic's source ranges highlight important elements in the
-- source code. On the command line, Clang displays source ranges by
-- underlining them with '~' characters.
getRanges :: ClangBase m => FFI.Diagnostic s' -> ClangT s m [FFI.SourceRange s]
getRanges d = liftIO $ do
                numRanges <- FFI.getDiagnosticNumRanges d
                mapM (FFI.getDiagnosticRange d) [0..(numRanges - 1)]

-- | Retrieve the fix-it hints associated with the given diagnostic.
--
-- Fix-its are described in terms of a source range whose contents
-- should be replaced by a string. This approach generalizes over
-- three kinds of operations: removal of source code (the range covers
-- the code to be removed and the replacement string is empty),
-- replacement of source code (the range covers the code to be
-- replaced and the replacement string provides the new code), and
-- insertion (both the start and end of the range point at the
-- insertion location, and the replacement string provides the text to
-- insert).
getFixIts :: ClangBase m
          => FFI.Diagnostic s'  -- ^ The diagnostic in question.
          -> ClangT s m [(FFI.SourceRange s, FFI.ClangString s)]  -- ^ A list of replacement
                                                                  --   ranges and strings. Note
                                                                  --   that source ranges are
                                                                  --   half-open ranges '[a, b)'
                                                                  --   so the source code should
                                                                  --   be replaced from 'a' up
                                                                  --   to (but not including)
                                                                  --   'b'.
getFixIts d = do
  numFixes <- liftIO $ FFI.getDiagnosticNumFixIts d
  mapM (FFI.getDiagnosticFixIt d) [0..(numFixes - 1)]

-- | Format the given diagnostic in a manner that is suitable for
-- display.
format :: ClangBase m
       => Maybe [FFI.DisplayOptions]     -- ^ An optional list of options
                                         --   for rendering the diagnostic.
                                         --   If 'Nothing' is given, default
                                         --   options are used that mimic the
                                         --   behavior of the Clang compiler
                                         --   as closely as possible.
       -> FFI.Diagnostic s'              -- ^ The diagnostic to format.
       -> ClangT s m (FFI.ClangString s) -- ^ A formatted rendering of the
                                         --   given diagnostic.
format Nothing diag     = FFI.formatDiagnostic diag =<<
                          liftIO FFI.defaultDiagnosticDisplayOptions
format (Just opts) diag = FFI.formatDiagnostic diag (orFlags opts)

-- | Deserialize a set of diagnostics from a Clang diagnostics bitcode file.
--
-- If an error is encountered, a 'FFI.LoadError' and a textual error
-- message suitable for display to the user are returned.
load :: ClangBase m
     => FilePath
     -> ClangT s m (Either (FFI.LoadError, FFI.ClangString s) (FFI.DiagnosticSet s))
load = FFI.loadDiagnostics
