{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Functions for manipulating translation units.
--
-- To start analyzing a translation unit, call 'Index.withNew' to create a new index and
-- call 'withParsed' in the callback. Inside the callback for 'withParsed', you'll have
-- access to a 'FFI.TranslationUnit' value; you can call 'getCursor' to turn that into an
-- AST cursor which can be traversed using the functions in "Clang.Cursor".
--
-- This module is intended to be imported qualified.
module Clang.TranslationUnit
(

-- * Creating a translation unit
  withParsed
, withLoaded
, withReparsing
, FFI.TranslationUnitFlags
, FFI.ReparseFlags
, ReparsingCallback
, ParseContinuation(..)

-- * Saving
, save
, FFI.SaveTranslationUnitFlags

-- AST traversal and metadata
, getCursor
, getDiagnosticSet
, getSpelling

) where

import Data.Traversable
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import qualified Data.Vector as DV
-- import System.FilePath ((</>))

import Clang.Internal.BitFlags
import Clang.Internal.Monad
import qualified Clang.Internal.FFI as FFI
-- import Paths_LibClang (getDataFileName)

-- | Creates a translation unit by parsing source code.
--
-- Note that not all command line arguments which are accepted by the \'clang\' frontend can
-- be used here. You should avoid passing \'-c\', \'-o\', \'-fpch-deps\', and the various
-- \'-M\' options. If you provide a 'FilePath' when calling 'withParsed', also be sure not
-- to provide the filename in the command line arguments as well.
withParsed :: ClangBase m
           => FFI.Index s'  -- ^ The index into which the translation unit should be loaded.
           -> Maybe FilePath  -- ^ The file to load, or 'Nothing' if the file is specified in
                              --   the command line arguments.
           -> [String]  -- ^ The command line arguments libclang should use when compiling this
                        --   file. Most arguments which you'd use with the \'clang\' frontend
                        --   are accepted.
           -> DV.Vector FFI.UnsavedFile  -- ^ Unsaved files which may be needed to parse this
                                         --   translation unit. This may include the source
                                         --   file itself or any file it includes.
           -> [FFI.TranslationUnitFlags]  -- ^ Flags that affect the processing of this
                                          --   translation unit.
           -> (forall s. FFI.TranslationUnit s -> ClangT s m a)  -- ^ A callback.
           -> ClangT s' m (Maybe a)  -- ^ The return value of the callback, or 'Nothing'
                                     --   if the file couldn't be parsed.
withParsed idx mayPath args ufs flags f = do
    -- liftIO $ FFI.setClangResourcesPath idx =<< clangResourcesPath
    mayTU <- FFI.parseTranslationUnit idx mayPath args ufs (orFlags flags)
    traverse go mayTU
  where
    go tu = clangScope $ f =<< fromOuterScope tu

-- | Creates a translation unit by loading a saved AST file.
--
-- Such an AST file can be created using 'save'.
withLoaded :: ClangBase m
           => FFI.Index s'  -- ^ The index into which the translation unit should be loaded.
           -> FilePath      -- ^ The file to load.
           -> (forall s. FFI.TranslationUnit s -> ClangT s m a)  -- ^ A callback.
           -> ClangT s' m a
withLoaded idx path f = do
  -- liftIO $ FFI.setClangResourcesPath idx =<< clangResourcesPath
  f =<< FFI.createTranslationUnit idx path


-- | Creates a translation unit by parsing source code.
--
-- This works like 'withParsed', except that the translation unit can be reparsed over and over
-- again by returning a 'Reparse' value from the callback. This is useful for interactive
-- analyses like code completion. Processing can be stopped by returning a 'ParseComplete' value.
withReparsing :: ClangBase m
              => FFI.Index s'  -- ^ The index into which the translation unit should be loaded.
              -> Maybe FilePath  -- ^ The file to load, or 'Nothing' if the file is specified in
                                 --   the command line arguments.
              -> [String]  -- ^ The command line arguments libclang should use when compiling
                           --   this file. Most arguments which you'd use with the \'clang\'
                           --   frontend are accepted.
              -> DV.Vector FFI.UnsavedFile  -- ^ Unsaved files which may be needed to parse this
                                            --   translation unit. This may include the source
                                            --   file itself or any file it includes.
              -> [FFI.TranslationUnitFlags]  -- ^ Flags that affect the processing of this
                                             --   translation unit.
              -> ReparsingCallback m r  -- ^ A callback which uses the translation unit. May be
                                        --   called many times depending on the return value.
                                        --   See 'ParseContinuation' for more information.
              -> ClangT s' m (Maybe r)  -- ^ The return value of the callback, as passed to
                                        --   'ParseComplete', or 'Nothing' if the file could
                                        --   not be parsed.
withReparsing idx mayPath args ufs flags f = do
    -- liftIO $ FFI.setClangResourcesPath idx =<< clangResourcesPath
    mayTU <- FFI.parseTranslationUnit idx mayPath args ufs (orFlags flags)
    case mayTU of
      Nothing -> return Nothing
      Just tu -> iterReparse f tu

iterReparse :: ClangBase m
            => ReparsingCallback m r
            -> FFI.TranslationUnit s'
            -> ClangT s' m (Maybe r)
iterReparse f tu = do
  cont <- clangScope $ f =<< fromOuterScope tu
  case cont of
    Reparse nextF ufs mayFlags ->
      do res <- FFI.reparseTranslationUnit tu ufs (makeFlags mayFlags)
         if res then iterReparse nextF tu
                else return Nothing
    ParseComplete finalVal -> return $ Just finalVal
  where
    makeFlags = orFlags . (fromMaybe [FFI.DefaultReparseFlags])


-- | A callback for use with 'withReparsing'.
type ReparsingCallback m r = forall s. FFI.TranslationUnit s
                          -> ClangT s m (ParseContinuation m r)

-- | A continuation returned by a 'ReparsingCallback'.
data ParseContinuation m r
  -- | 'Reparse' signals that the translation unit should be reparsed. It contains a callback
  -- which will be called with the updated translation unit after reparsing, a 'DV.Vector' of
  -- unsaved files which may be needed to reparse, and a set of flags affecting reparsing. The
  -- default reparsing flags can be requested by specifying 'Nothing'.
  = Reparse (ReparsingCallback m r) (DV.Vector FFI.UnsavedFile) (Maybe [FFI.ReparseFlags])

  -- | 'ParseComplete' signals that processing is finished. It contains a final result which
  -- will be returned by 'withReparsing'.
  | ParseComplete r

-- clangResourcesPath :: IO FilePath
-- clangResourcesPath =
--   getDataFileName $ "build" </> "out" </> "lib" </> "clang" </> "3.4"

-- | Saves a translation unit as an AST file with can be loaded later using 'withLoaded'.
save :: ClangBase m
     => FFI.TranslationUnit s'  -- ^ The translation unit to save.
     -> FilePath                -- ^ The filename to save to.
     -> Maybe [FFI.SaveTranslationUnitFlags]  -- ^ Flags that affect saving, or 'Nothing' for
                                              --   the default set of flags.
     -> ClangT s m Bool
save t fname mayFlags = liftIO $ FFI.saveTranslationUnit t fname (orFlags flags)
  where flags = fromMaybe [FFI.DefaultSaveTranslationUnitFlags] mayFlags

{-
-- | Reparses the provided translation unit using the same command line arguments
-- that were originally used to parse it. If the file has changed on disk, or if
-- the unsaved files have changed, those changes will become visible.
--
-- Note that 'reparse' invalidates all cursors and source locations that refer into
-- the reparsed translation unit. This makes it unsafe. However, 'reparse' can be
-- more efficient than calling 'withParsed' a second time.
reparse :: ClangBase m
        => FFI.TranslationUnit s'    -- ^ The translation unit to reparse.
        -> DV.Vector FFI.UnsavedFile -- ^ Unsaved files which may be needed to reparse
                                     --   this translation unit.
        -> Maybe [FFI.ReparseFlags]  -- ^ Flags that affect reparsing, or 'Nothing' for the
                                     --   default set of flags.
        -> ClangT s m Bool
reparse t ufs mayFlags = FFI.reparseTranslationUnit t ufs (orFlags flags)
  where flags = fromMaybe [FFI.DefaultReparseFlags] mayFlags
-}

-- | Retrieve the cursor associated with this translation unit. This cursor is the root of
-- this translation unit's AST; you can begin exploring the AST further using the functions
-- in "Clang.Cursor".
getCursor :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.Cursor s)
getCursor tu = liftIO $ FFI.getTranslationUnitCursor mkProxy tu

-- | Retrieve the complete set of diagnostics associated with the given translation unit.
getDiagnosticSet :: ClangBase m => FFI.TranslationUnit s'-> ClangT s m (FFI.DiagnosticSet s)
getDiagnosticSet = FFI.getDiagnosticSetFromTU

-- | Retrieve a textual representation of this translation unit.
getSpelling :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.ClangString s)
getSpelling = FFI.getTranslationUnitSpelling
