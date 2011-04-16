module Clang.TranslationUnit
(
 FFI.Index
,FFI.TranslationUnitFlags(..)
,FFI.SaveTranslationUnitFlags(..)
,getSpelling
,withCreateIndex
,withCreate
,withCreateFromSourceFile
,withParse
,defaultSaveOptions
,save
,defaultReparseOptions
,reparse
) where

import System.IO.Unsafe(unsafePerformIO)
import Data.Bits((.&.))
import Data.Maybe(catMaybes)
import Control.Monad(mzero, (<=<))
import Foreign.ForeignPtr(withForeignPtr)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

getSpelling = unsafePerformIO . FFI.getTranslationUnitSpelling
withCreate i s f = flip withForeignPtr f =<< FFI.createTranslationUnit i s

withCreateFromSourceFile :: FFI.Index -- ^ Index for the source
                     -> FilePath -- ^ Source filename
                     -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
                     -> [FFI.UnsavedFile] -- ^ Unsaved files
                     -> (FFI.TranslationUnit -> IO a) -- ^ Function that will process the TranslationUnit
                     -> IO a
withCreateFromSourceFile i fn ss ufs f = flip withForeignPtr f =<< FFI.createTranslationUnitFromSourceFile i fn ss ufs

withParse :: FFI.Index -- ^ Index for the source
      -> Maybe FilePath -- ^ Source filename
      -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
      -> [FFI.UnsavedFile] -- ^ Unsaved files
      -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
      -> (FFI.TranslationUnit -> IO a) -- ^ Function that will process the TranslationUnit
      -> IO a -- ^ Result to be returned if source couldn't be parsed
      -> IO a
withParse i ms ss ufs opts f nr = maybe nr (flip withForeignPtr f) =<< 
                                  FFI.parseTranslationUnit i ms ss ufs (FFI.getTranslationUnitFlagsSum opts)

-- No other option right now
defaultSaveOptions = [FFI.SaveTranslationUnit_None]

save :: FFI.TranslationUnit -- ^ TranslationUnit to save
     -> FilePath -- ^ Filename to save to
     -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
     -> IO Bool
save t fname opts = FFI.saveTranslationUnit t fname (FFI.getSaveTranslationUnitFlagsSum opts)

-- No other option right now
defaultReparseOptions = [FFI.Reparse_None]

reparse :: FFI.TranslationUnit -- ^ TranslationUnit to save
        -> [FFI.UnsavedFile] -- ^ All the unsaved files
        -> [FFI.ReparseFlags] -- ^ reparse options
        -> IO Bool
reparse t ufs opts = FFI.reparseTranslationUnit t ufs (FFI.getReparseFlagsSum opts)

-- index functions
withCreateIndex i1 i2 f = flip withForeignPtr f =<< FFI.createIndex i1 i2
