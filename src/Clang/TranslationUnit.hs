module Clang.TranslationUnit
(
 FFI.Index
,FFI.TranslationUnitFlags(..)
,create
,getSpelling
,createFromSourceFile
,parse
) where

import System.IO.Unsafe(unsafePerformIO)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

getSpelling = unsafePerformIO . FFI.getTranslationUnitSpelling
create = FFI.createTranslationUnit
createFromSourceFile :: FFI.Index -- ^ Index for the source
                     -> String -- ^ Source filename
                     -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
                     -> [FFI.UnsavedFile] -- ^ Unsaved files
                     -> IO FFI.TranslationUnit
createFromSourceFile = FFI.createTranslationUnitFromSourceFile
parse :: FFI.Index -- ^ Index for the source
      -> Maybe String -- ^ Source filename
      -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
      -> [FFI.UnsavedFile] -- ^ Unsaved files
      -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
      -> IO (Maybe FFI.TranslationUnit)
parse i ms ss ufs opts = FFI.parseTranslationUnit i ms ss ufs (FFI.getTranslationUnitFlagsSum opts)