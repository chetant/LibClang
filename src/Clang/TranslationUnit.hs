{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Clang.TranslationUnit
( getSpelling
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
, getGlobalOptions
, FFI.unsavedFilename
, FFI.unsavedContents
, FFI.newUnsavedFile
, FFI.updateUnsavedContents
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Traversable (traverse)
import qualified Data.Vector as DV
import System.FilePath ((</>))

import Clang.Internal.BitFlags
import Clang.Internal.Monad
import qualified Clang.Internal.FFI as FFI
import Paths_LibClang (getDataFileName)

getSpelling :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.ClangString s)
getSpelling = FFI.getTranslationUnitSpelling

withCreate :: ClangBase m => FFI.Index s' -> String
           -> (FFI.TranslationUnit s -> ClangT s m a)
           -> ClangT s m a
withCreate idx str f = do
  liftIO $ FFI.setClangResourcesPath idx =<< clangResourcesPath
  f =<< FFI.createTranslationUnit idx str

withCreateFromSourceFile ::
     ClangBase m =>
     FFI.Index s' -- ^ Index for the source
  -> FilePath -- ^ Source filename
  -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
  -> DV.Vector FFI.UnsavedFile -- ^ Unsaved files
  -> (FFI.TranslationUnit s -> ClangT s m a) -- ^ Function that will process the TranslationUnit
  -> ClangT s m a
withCreateFromSourceFile idx fn ss ufs f = do
  liftIO $ FFI.setClangResourcesPath idx =<< clangResourcesPath
  f =<< FFI.createTranslationUnitFromSourceFile idx fn ss ufs

withParse ::
     ClangBase m =>
     FFI.Index s' -- ^ Index for the source
  -> Maybe FilePath -- ^ Source filename
  -> [String] -- ^ Command line arguments ( this can include all clang compatible flags)
  -> DV.Vector FFI.UnsavedFile -- ^ Unsaved files
  -> [FFI.TranslationUnitFlags] -- ^ TranslationUnit flags
  -> (FFI.TranslationUnit s -> ClangT s m a) -- ^ Function that will process the TranslationUnit
  -> ClangT s m (Maybe a)
withParse idx ms ss ufs opts f = do
  liftIO $ FFI.setClangResourcesPath idx =<< clangResourcesPath
  mayTU <- FFI.parseTranslationUnit idx ms ss ufs (orFlags opts)
  traverse f mayTU

clangResourcesPath :: IO FilePath
clangResourcesPath =
  getDataFileName $ "build" </> "out" </> "lib" </> "clang" </> "3.4"

-- No other option right now
defaultSaveOptions :: ClangBase m => ClangT s m [FFI.SaveTranslationUnitFlags]
defaultSaveOptions = return [FFI.SaveTranslationUnit_None]

save ::
     ClangBase m =>
     FFI.TranslationUnit s' -- ^ TranslationUnit to save
  -> FilePath -- ^ Filename to save to
  -> [FFI.SaveTranslationUnitFlags] -- ^ Saving Flags
  -> ClangT s m Bool
save t fname opts = liftIO $ FFI.saveTranslationUnit t fname (orFlags opts)

-- No other option right now
defaultReparseOptions :: ClangBase m => ClangT s m [FFI.ReparseFlags]
defaultReparseOptions = return [FFI.Reparse_None]

reparse ::
     ClangBase m =>
     FFI.TranslationUnit s' -- ^ TranslationUnit to save
  -> DV.Vector FFI.UnsavedFile -- ^ All the unsaved files
  -> [FFI.ReparseFlags] -- ^ reparse options
  -> ClangT s m Bool
reparse t ufs opts = FFI.reparseTranslationUnit t ufs (orFlags opts)

getCursor :: ClangBase m => FFI.TranslationUnit s' -> ClangT s m (FFI.Cursor s)
getCursor tu = liftIO $ FFI.getTranslationUnitCursor mkProxy tu

-- index functions
withCreateIndex :: ClangBase m => Bool -> Bool -> (forall s. FFI.Index s -> ClangT s m a) -> m a
withCreateIndex i1 i2 f = runClangT (FFI.createIndex i1 i2 >>= f)

setGlobalOptions :: ClangBase m => FFI.Index s' -> [FFI.GlobalOptFlags] -> ClangT s m ()
setGlobalOptions i opts = liftIO $ FFI.cXIndex_setGlobalOptions i (orFlags opts)

getGlobalOptions :: ClangBase m => FFI.Index s' -> ClangT s m [FFI.GlobalOptFlags]
getGlobalOptions i = unFlags <$> liftIO (FFI.cXIndex_getGlobalOptions i)
