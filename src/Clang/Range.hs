{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'FFI.SourceRange's, which represent a range of
-- locations in the source code between two points.
--
-- This module is intended to be imported qualified.
module Clang.Range
(

-- * Creating ranges
  create
, createInvalid

-- * Analyzing ranges
, getStart
, getEnd
, isInvalid

) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad
import System.IO.Unsafe (unsafePerformIO)

-- | Creates a 'FFI.SourceRange' representing the range between two locations.
create :: ClangBase m
       => FFI.SourceLocation s'   -- ^ Starting location.
       -> FFI.SourceLocation s''  -- ^ Ending location.
       -> ClangT s m (FFI.SourceRange s)
create from to = liftIO $ FFI.getRange mkProxy from to

-- | Creates an invalid 'FFI.SourceRange'.
createInvalid :: ClangBase m => ClangT s m (FFI.SourceRange s)
createInvalid = liftIO $ FFI.getNullRange mkProxy

-- | Retrieves the beginning of the given range.
getStart :: ClangBase m => FFI.SourceRange s' -> ClangT s m (FFI.SourceLocation s)
getStart sr = liftIO $ FFI.getRangeStart mkProxy sr

-- | Retrieves the end of the given range.
getEnd :: ClangBase m => FFI.SourceRange s' -> ClangT s m (FFI.SourceLocation s)
getEnd sr = liftIO $ FFI.getRangeEnd mkProxy sr

-- | Returns 'True' if the given range is invalid.
isInvalid :: FFI.SourceRange s' -> Bool
isInvalid r = unsafePerformIO $ FFI.range_isNull r
