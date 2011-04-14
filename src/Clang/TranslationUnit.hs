module Clang.TranslationUnit
(
 FFI.Index
,FFI.TranslationUnitFlags(..)
,FFI.SaveTranslationUnitFlags(..)
,create
,createIndex
,getSpelling
,createFromSourceFile
,parse
,defaultSaveOptions
,save
,defaultReparseOptions
,reparse
) where

import System.IO.Unsafe(unsafePerformIO)
import Data.Bits((.&.))
import Data.Maybe(catMaybes)
import Control.Monad(mzero)
import Foreign.ForeignPtr(withForeignPtr)

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

-- No other option right now
defaultSaveOptions = [FFI.SaveTranslationUnit_None]

save :: FFI.TranslationUnit -- ^ TranslationUnit to save
     -> FilePath -- ^ Filename to save to
     -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
     -> IO Bool
save t fname opts = FFI.saveTranslationUnit t fname (FFI.getSaveTranslationUnitFlagsSum opts)

-- No other option right now
defaultReparseOptions = [FFI.Reparse_None]
    -- where defVal = unsafePerformIO . FFI.defaultReparseOptions
    --       val1 v = if (v .&. 0x1) == 0x1 then return FFI.Diagnostic_DisplaySourceLocation else mzero

reparse :: FFI.TranslationUnit -- ^ TranslationUnit to save
        -> [FFI.UnsavedFile] -- ^ All the unsaved files
        -> [FFI.ReparseFlags] -- ^ reparse options
        -> IO Bool
reparse t ufs opts = FFI.reparseTranslationUnit t ufs (FFI.getReparseFlagsSum opts)

-- index functions
withCreateIndex i1 i2 f = do
  i <- FFI.createIndex i1 i2
  withForeignPtr i f
