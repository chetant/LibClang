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
, defaultSaveOptions
, save
, defaultReparseOptions
, reparse
, getCursor
, setGlobalOptions
) where

import Control.Monad.IO.Class

import Clang.Internal.ClangApp
import qualified Clang.Internal.FFI as FFI

getSpelling :: FFI.TranslationUnit -> ClangApp s FFI.CXString
getSpelling tu = FFI.registerCXString $ FFI.getTranslationUnitSpelling tu

withCreate :: FFI.Index -> String -> (FFI.TranslationUnit -> ClangApp s a) -> ClangApp s a
withCreate idx str f = do
  tu <- FFI.registerTranslationUnit $ FFI.createTranslationUnit idx str
  f tu

withCreateFromSourceFile :: FFI.Index -- ^ Index for the source
                     -> FilePath -- ^ Source filename
                     -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
                     -> [FFI.UnsavedFile] -- ^ Unsaved files
                     -> (FFI.TranslationUnit -> ClangApp s a) -- ^ Function that will process the TranslationUnit
                     -> ClangApp s a
withCreateFromSourceFile idx fn ss ufs f = do
    tu <- FFI.registerTranslationUnit $ create idx fn ss ufs
    f tu
  where create = FFI.createTranslationUnitFromSourceFile

withParse :: FFI.Index -- ^ Index for the source
      -> Maybe FilePath -- ^ Source filename
      -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
      -> [FFI.UnsavedFile] -- ^ Unsaved files
      -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
      -> (FFI.TranslationUnit -> ClangApp s a) -- ^ Function that will process the TranslationUnit
      -> ClangApp s a -- ^ Result to be returned if source couldn't be parsed
      -> ClangApp s a
withParse idx ms ss ufs opts f nr = do
    tu <- liftIO $ FFI.parseTranslationUnit idx ms ss ufs flags
    maybe nr run tu
  where flags = FFI.getTranslationUnitFlagsSum opts
        run t = do tu <- FFI.registerTranslationUnit (return t)
                   f tu

-- No other option right now
defaultSaveOptions :: ClangApp s [FFI.SaveTranslationUnitFlags]
defaultSaveOptions = return [FFI.SaveTranslationUnit_None]

save :: FFI.TranslationUnit -- ^ TranslationUnit to save
     -> FilePath -- ^ Filename to save to
     -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
     -> ClangApp s Bool
save t fname opts = liftIO $ FFI.saveTranslationUnit t fname (FFI.getSaveTranslationUnitFlagsSum opts)

-- No other option right now
defaultReparseOptions :: ClangApp s [FFI.ReparseFlags]
defaultReparseOptions = return [FFI.Reparse_None]

reparse :: FFI.TranslationUnit -- ^ TranslationUnit to save
        -> [FFI.UnsavedFile] -- ^ All the unsaved files
        -> [FFI.ReparseFlags] -- ^ reparse options
        -> ClangApp s Bool
reparse t ufs opts = liftIO $ FFI.reparseTranslationUnit t ufs (FFI.getReparseFlagsSum opts)

getCursor :: FFI.TranslationUnit -> ClangApp s FFI.Cursor
getCursor tu = liftIO $ FFI.getTranslationUnitCursor tu

-- index functions
withCreateIndex :: Bool -> Bool -> (forall s. FFI.Index -> ClangApp s a) -> IO a
withCreateIndex i1 i2 f = runClangApp (FFI.registerIndex (FFI.createIndex i1 i2) >>= f)

setGlobalOptions :: FFI.Index -> FFI.GlobalOptFlags -> ClangApp s ()
setGlobalOptions i fs = liftIO $ FFI.cXIndex_setGlobalOptions i fs
