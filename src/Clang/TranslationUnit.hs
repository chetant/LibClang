module Clang.TranslationUnit
( Index
, FFI.Cursor
, FFI.ReparseFlags
, FFI.SaveTranslationUnitFlags(..)
, FFI.TranslationUnit
, FFI.TranslationUnitFlags(..)
, FFI.UnsavedFile
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
) where

import Control.Monad.IO.Class
import Foreign.ForeignPtr

import Clang.Internal.ClangApp
import qualified Clang.Internal.FFI as FFI

newtype Index = Index FFI.IndexPtr
  deriving (Eq, Show)

getSpelling :: FFI.TranslationUnit -> ClangApp FFI.CXString
getSpelling tu = FFI.registerCXString $ FFI.getTranslationUnitSpelling tu

withCreate :: Index -> String -> ClangApp a -> IO a
withCreate (Index i) s f = runClangApp f i =<< withForeignPtr i create
  where create idx = FFI.createTranslationUnit idx s

withCreateFromSourceFile :: Index -- ^ Index for the source
                     -> FilePath -- ^ Source filename
                     -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
                     -> [FFI.UnsavedFile] -- ^ Unsaved files
                     -> ClangApp a -- ^ Function that will process the TranslationUnit
                     -> IO a
withCreateFromSourceFile (Index i) fn ss ufs f = runClangApp f i =<< withForeignPtr i create
  where create idx = FFI.createTranslationUnitFromSourceFile idx fn ss ufs

withParse :: Index -- ^ Index for the source
      -> Maybe FilePath -- ^ Source filename
      -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
      -> [FFI.UnsavedFile] -- ^ Unsaved files
      -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
      -> (ClangApp a) -- ^ Function that will process the TranslationUnit
      -> IO a -- ^ Result to be returned if source couldn't be parsed
      -> IO a
withParse (Index i) ms ss ufs opts f nr = maybe nr (runClangApp f i) =<< withForeignPtr i parse
  where parse idx = FFI.parseTranslationUnit idx ms ss ufs (FFI.getTranslationUnitFlagsSum opts)

-- No other option right now
defaultSaveOptions :: ClangApp [FFI.SaveTranslationUnitFlags]
defaultSaveOptions = return [FFI.SaveTranslationUnit_None]

save :: FFI.TranslationUnit -- ^ TranslationUnit to save
     -> FilePath -- ^ Filename to save to
     -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
     -> ClangApp Bool
save t fname opts = liftIO $ FFI.saveTranslationUnit t fname (FFI.getSaveTranslationUnitFlagsSum opts)

-- No other option right now
defaultReparseOptions :: ClangApp [FFI.ReparseFlags]
defaultReparseOptions = return [FFI.Reparse_None]

reparse :: FFI.TranslationUnit -- ^ TranslationUnit to save
        -> [FFI.UnsavedFile] -- ^ All the unsaved files
        -> [FFI.ReparseFlags] -- ^ reparse options
        -> ClangApp Bool
reparse t ufs opts = liftIO $ FFI.reparseTranslationUnit t ufs (FFI.getReparseFlagsSum opts)

getCursor :: FFI.TranslationUnit -> ClangApp FFI.Cursor
getCursor tu = liftIO $ FFI.getTranslationUnitCursor tu

-- index functions
withCreateIndex :: Bool -> Bool -> (Index -> IO a) -> IO a
withCreateIndex i1 i2 f = f . Index =<< FFI.createIndex i1 i2
