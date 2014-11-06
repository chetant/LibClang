-- | Functions for working with unsaved files.
--
-- Unsaved files are used to represent files which are in memory, but haven't
-- yet been written to disk. You'll need to use unsaved files for situations
-- like supporting autocomplete in an editor without requiring the user to save
-- for up-to-date completions. See "Clang.TranslationUnit" for details about
-- using unsaved files.
--
-- This module is intended to be imported qualified.
module Clang.UnsavedFile
( 

-- * Creating an unsaved file
  new

-- * Field accessors
, filename
, contents

-- * Updates
, updateContents

) where

import qualified Data.ByteString as B

import qualified Clang.Internal.FFI as FFI

-- | Create a new unsaved file.
new :: B.ByteString -> B.ByteString -> FFI.UnsavedFile
new = FFI.newUnsavedFile

-- | Retrieve the filename of an unsaved file.
filename :: FFI.UnsavedFile -> B.ByteString
filename = FFI.unsavedFilename

-- | Retrieve the contents of an unsaved file.
contents :: FFI.UnsavedFile -> B.ByteString
contents = FFI.unsavedContents

-- | Update the contents of an unsaved file. This is more efficient than creating
-- a new unsaved file with the same filename.
updateContents :: FFI.UnsavedFile -> B.ByteString -> FFI.UnsavedFile
updateContents = FFI.updateUnsavedContents
