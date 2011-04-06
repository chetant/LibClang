module Clang.TranslationUnit
(
 FFI.Index
,FFI.TranslationUnitFlags(..)
,FFI.SaveTranslationUnitFlags(..)
,create
,getSpelling
,createFromSourceFile
,parse
,defaultSaveOptions
,save
,defaultReparseOptions
,reparse
) where

import System.IO.Unsafe(unsafePerformIO)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

getSpelling = unsafePerformIO . FFI.getTranslationUnitSpelling
create = FFI.createTranslationUnit
createFromSourceFile :: FFI.Index -- ^ Index for the source
                     -> FilePath -- ^ Source filename
                     -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
                     -> [FFI.UnsavedFile] -- ^ Unsaved files
                     -> IO FFI.TranslationUnit
createFromSourceFile = FFI.createTranslationUnitFromSourceFile
parse :: FFI.Index -- ^ Index for the source
      -> Maybe FilePath -- ^ Source filename
      -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
      -> [FFI.UnsavedFile] -- ^ Unsaved files
      -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
      -> IO (Maybe FFI.TranslationUnit)
parse i ms ss ufs opts = FFI.parseTranslationUnit i ms ss ufs (FFI.getTranslationUnitFlagsSum opts)

defaultSaveOptions = unsafePerformIO . FFI.defaultSaveOptions

save :: FFI.TranslationUnit -- ^ TranslationUnit to save
     -> FilePath -- ^ Filename to save to
     -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
     -> IO Bool
save t fname opts = FFI.saveTranslationUnit t fname (FFI.getSaveTranslationUnitFlagsSum opts)

defaultReparseOptions = unsafePerformIO . FFI.defaultReparseOptions
reparse :: FFI.TranslationUnit -- ^ TranslationUnit to save
        -> [FFI.UnsavedFile] -- ^ All the unsaved files
        -> [FFI.ReparseFlags] -- ^ reparse options
        -> IO Bool
reparse t ufs opts = FFI.reparseTranslationUnit t ufs (FFI.getReparseFlagsSum opts)
