{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating remappings.
module Clang.Remapping
( getRemappings
, getRemappingsFromFileList
, getRemappedFiles
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Given a 'FilePath' for a file, return information about the remappings that
-- apply to that file. If an error occurs, returns 'Nothing'.
getRemappings :: ClangBase m => FilePath -> ClangT s m (Maybe (FFI.Remapping s))
getRemappings = FFI.getRemappings

-- | Given a list of 'FilePath's, return information about the
-- remappings that apply to the corresponding files. If an error
-- occurs, returns 'Nothing'.
getRemappingsFromFileList :: ClangBase m => [FilePath] -> ClangT s m (Maybe (FFI.Remapping s))
getRemappingsFromFileList = FFI.getRemappingsFromFileList

-- | Retrives a list of the remappings described by a
-- 'FFI.Remapping'. Each entry in the list consists of a pair where
-- the first element is the original filename, and the second element
-- is the filename it has been remapped to.
getRemappedFiles :: ClangBase m => FFI.Remapping s'
                 -> ClangT s m [(FFI.ClangString s, FFI.ClangString s)]
getRemappedFiles remaps = do
  numRemaps <- liftIO $ FFI.remap_getNumFiles remaps
  mapM (FFI.remap_getFilenames remaps) [0..(numRemaps - 1)]
