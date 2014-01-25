{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Clang.TranslationUnit
( FFI.Index
, FFI.Cursor
, FFI.ReparseFlags
, FFI.SaveTranslationUnitFlags(..)
, FFI.TranslationUnit
, FFI.TranslationUnitFlags(..)
, FFI.UnsavedFile
, FFI.GlobalOptFlags(..)
, getSpelling
, withCreateIndex
, withCreate
, withCreateFromSourceFile
, withParse
, libclangIncludePath
, defaultSaveOptions
, save
, defaultReparseOptions
, reparse
, getCursor
, setGlobalOptions
) where

import Control.Monad.IO.Class
import System.FilePath ((</>))

import Clang.Internal.Monad
import qualified Clang.Internal.FFI as FFI
import Paths_LibClang (getDataFileName)

getSpelling :: ClangBase m => FFI.TranslationUnit -> ClangT s m FFI.CXString
getSpelling tu = FFI.registerCXString $ FFI.getTranslationUnitSpelling tu

withCreate :: ClangBase m => FFI.Index -> String
           -> (FFI.TranslationUnit -> ClangT s m a)
           -> ClangT s m a
withCreate idx str f = do
  tu <- FFI.registerTranslationUnit $ FFI.createTranslationUnit idx str
  f tu

withCreateFromSourceFile ::
     ClangBase m =>
     FFI.Index -- ^ Index for the source
  -> FilePath -- ^ Source filename
  -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
  -> [FFI.UnsavedFile] -- ^ Unsaved files
  -> (FFI.TranslationUnit -> ClangT s m a) -- ^ Function that will process the TranslationUnit
  -> ClangT s m a
withCreateFromSourceFile idx fn ss ufs f = do
    tu <- FFI.registerTranslationUnit $ create idx fn ss ufs
    f tu
  where create = FFI.createTranslationUnitFromSourceFile

withParse ::
     ClangBase m =>
     FFI.Index -- ^ Index for the source
  -> Maybe FilePath -- ^ Source filename
  -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
  -> [FFI.UnsavedFile] -- ^ Unsaved files
  -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
  -> (FFI.TranslationUnit -> ClangT s m a) -- ^ Function that will process the TranslationUnit
  -> ClangT s m a -- ^ Result to be returned if source couldn't be parsed
  -> ClangT s m a
withParse idx ms ss ufs opts f nr = do
    tu <- liftIO $ FFI.parseTranslationUnit idx ms ss ufs flags
    maybe nr run tu
  where flags = FFI.getTranslationUnitFlagsSum opts
        run t = do tu <- FFI.registerTranslationUnit (return t)
                   f tu

libclangIncludePath :: ClangBase m => m FilePath
libclangIncludePath = liftIO $
  getDataFileName $ "build" </> "out" </> "lib" </> "clang" </> "3.4" </> "include"

-- No other option right now
defaultSaveOptions :: ClangBase m => ClangT s m [FFI.SaveTranslationUnitFlags]
defaultSaveOptions = return [FFI.SaveTranslationUnit_None]

save ::
     ClangBase m =>
     FFI.TranslationUnit -- ^ TranslationUnit to save
  -> FilePath -- ^ Filename to save to
  -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
  -> ClangT s m Bool
save t fname opts = liftIO $
  FFI.saveTranslationUnit t fname (FFI.getSaveTranslationUnitFlagsSum opts)

-- No other option right now
defaultReparseOptions :: ClangBase m => ClangT s m [FFI.ReparseFlags]
defaultReparseOptions = return [FFI.Reparse_None]

reparse ::
     ClangBase m =>
     FFI.TranslationUnit -- ^ TranslationUnit to save
  -> [FFI.UnsavedFile] -- ^ All the unsaved files
  -> [FFI.ReparseFlags] -- ^ reparse options
  -> ClangT s m Bool
reparse t ufs opts = liftIO $ FFI.reparseTranslationUnit t ufs (FFI.getReparseFlagsSum opts)

getCursor :: ClangBase m => FFI.TranslationUnit -> ClangT s m FFI.Cursor
getCursor tu = liftIO $ FFI.getTranslationUnitCursor tu

-- index functions
withCreateIndex :: ClangBase m => Bool -> Bool -> (forall s. FFI.Index -> ClangT s m a) -> m a
withCreateIndex i1 i2 f = runClangT (FFI.registerIndex (FFI.createIndex i1 i2) >>= f)

setGlobalOptions :: ClangBase m => FFI.Index -> FFI.GlobalOptFlags -> ClangT s m ()
setGlobalOptions i fs = liftIO $ FFI.cXIndex_setGlobalOptions i fs
