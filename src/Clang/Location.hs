{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'FFI.SourceLocation's, which map a location in a
-- translation unit to a location in on-disk or in-memory files.
--
-- This module is intended to be imported qualified.
module Clang.Location
(

-- * Creating locations
  create
, createForOffset
, createInvalid

-- * Mapping locations to files
, getExpansionLocation
, getPresumedLocation
, getSpellingLocation
, getFileLocation

-- * Analyzing locations
, getCursor
, isInSystemHeader
, isFromMainFile

) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad
import System.IO.Unsafe (unsafePerformIO)

-- | Retrieves a 'FFI.SourceLocation' representing the given location in the source code.
create :: ClangBase m
       => FFI.TranslationUnit s'  -- ^ The translation unit.
       -> FFI.File s''            -- ^ The file.
       -> Int                     -- ^ The line.
       -> Int                     -- ^ The column.
       -> ClangT s m (FFI.SourceLocation s)  -- ^ A 'FFI.SourceLocation' value representing
                                                  -- the location.
create tu f line col = liftIO $ FFI.getLocation mkProxy tu f line col

-- | Like 'create', but uses an offset instead of a line and column.
createForOffset :: ClangBase m
                => FFI.TranslationUnit s'  -- ^ The translation unit.
                -> FFI.File s''            -- ^ The file.
                -> Int                     -- ^ The offset.
                -> ClangT s m (FFI.SourceLocation s)  -- ^ A 'FFI.SourceLocation' value
                                                           -- representing the location.
createForOffset tu f off = liftIO $ FFI.getLocationForOffset mkProxy tu f off

-- | Creates an invalid 'FFI.SourceLocation'.
createInvalid :: ClangBase m => ClangT s m (FFI.SourceLocation s)
createInvalid = liftIO $ FFI.getNullLocation mkProxy

-- | Retrieves the 'FFI.File', line, column, and offset associated with the given location.
--
-- If the location points into a macro expansion, retrieves the location of the macro expansion.
-- This may be a position that doesn't exist in the original source.
getExpansionLocation :: ClangBase m => FFI.SourceLocation s'
                     -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getExpansionLocation l = liftIO $ FFI.getExpansionLocation mkProxy l

-- | Retrieves the 'FFI.File', line, and column associated with the given location, as
-- interpreted by the C preprocessor.
--
-- This may differ from the results given by 'getExpansionLocation' because it takes '#line'
-- directives into account, which 'getExpansionLocation' ignores.
getPresumedLocation :: ClangBase m => FFI.SourceLocation s'
                    -> ClangT s m (FFI.ClangString s, Int, Int)
getPresumedLocation = FFI.getPresumedLocation

-- | Retrieves the 'FFI.File', line, column, and offset associated with the given location.
--
-- If the location points into a macro expansion, returns the corresponding position in the
-- original source. This may be where the macro was defined or where it was instantiated,
-- depending on what exactly the location points to.
getSpellingLocation :: ClangBase m => FFI.SourceLocation s'
                    -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getSpellingLocation mkProxy l

-- | Retrieves the 'FFI.File', line, column, and offset associated with the given location.
--
-- If the location points into a macro expansion, returns the position where the macro was
-- expanded or the position of the macro argument, if the cursor points at a macro argument.
getFileLocation :: ClangBase m => FFI.SourceLocation s'
                    -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getFileLocation l = liftIO $ FFI.getFileLocation mkProxy l

-- | Retrieves an AST cursor at the given source location.
--
-- The cursor can be traversed using the functions in "Clang.Cursor".
getCursor :: ClangBase m => FFI.TranslationUnit s' -> FFI.SourceLocation s''
          -> ClangT s m (FFI.Cursor s)
getCursor tu sl = liftIO $ FFI.getCursor mkProxy tu sl

-- | Returns 'True' if the given location is inside a system header.
isInSystemHeader :: FFI.SourceLocation s' -> Bool
isInSystemHeader l = unsafePerformIO $ FFI.location_isInSystemHeader l

-- | Returns 'True' if the given location is from the main file of the associated translation
-- unit.
isFromMainFile :: FFI.SourceLocation s' -> Bool
isFromMainFile l = unsafePerformIO $ FFI.location_isFromMainFile l
