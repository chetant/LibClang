{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Remapping
( FFI.Remapping
, getRemappings
, getRemappingsFromFileList
, getRemappedFiles
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

getRemappings :: ClangBase m => FilePath -> ClangT s m (Maybe (FFI.Remapping s))
getRemappings = FFI.getRemappings

getRemappingsFromFileList :: ClangBase m => [FilePath] -> ClangT s m (Maybe (FFI.Remapping s))
getRemappingsFromFileList = FFI.getRemappingsFromFileList

getRemappedFiles :: ClangBase m => FFI.Remapping s'
                 -> ClangT s m [(ClangString s, ClangString s)]
getRemappedFiles remaps = do
  numRemaps <- liftIO $ FFI.remap_getNumFiles remaps
  mapM (FFI.remap_getFilenames remaps) [0..(numRemaps - 1)]
