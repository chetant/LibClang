{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Source
( FFI.SourceLocation
, nullLocation
, isSameLocation
, getLocation
, getLocationForOffset
, getCursor
, FFI.SourceRange
, nullRange
, getRange
, getExpansionLocation
, getInstantiationLocation
, getSpellingLocation
, getStart
, getEnd
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

-- Location functions
nullLocation :: ClangBase m => ClangT s m (FFI.SourceLocation s)
nullLocation = FFI.getNullLocation

isSameLocation :: ClangBase m => FFI.SourceLocation s -> FFI.SourceLocation s -> ClangT s m Bool
isSameLocation a b = liftIO $ FFI.equalLocations a b

getLocation :: ClangBase m => FFI.TranslationUnit s -> FFI.File -> Int -> Int
            -> ClangT s m (FFI.SourceLocation s)
getLocation tu f line col = liftIO $ FFI.getLocation tu f line col

getLocationForOffset :: ClangBase m => FFI.TranslationUnit s -> FFI.File -> Int
                     -> ClangT s m (FFI.SourceLocation s)
getLocationForOffset tu f off = liftIO $ FFI.getLocationForOffset tu f off

getCursor :: ClangBase m => FFI.TranslationUnit s -> FFI.SourceLocation s
          -> ClangT s m FFI.Cursor
getCursor tu sl = liftIO $ FFI.getCursor tu sl

-- Range functions
nullRange :: ClangBase m => ClangT s m (FFI.SourceRange s)
nullRange = FFI.getNullRange

getRange :: ClangBase m => FFI.SourceLocation s -> FFI.SourceLocation s
         -> ClangT s m (FFI.SourceRange s)
getRange from to = liftIO $ FFI.getRange from to

getExpansionLocation :: ClangBase m => FFI.SourceLocation s
                     -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getExpansionLocation l = liftIO $ FFI.getExpansionLocation l

getInstantiationLocation :: ClangBase m => FFI.SourceLocation s
                         -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getInstantiationLocation l = liftIO $ FFI.getInstantiationLocation l

getSpellingLocation :: ClangBase m => FFI.SourceLocation s
                    -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getSpellingLocation l

getStart :: ClangBase m => FFI.SourceRange s -> ClangT s m (FFI.SourceLocation s)
getStart sr = liftIO $ FFI.getRangeStart sr

getEnd :: ClangBase m => FFI.SourceRange s -> ClangT s m (FFI.SourceLocation s)
getEnd sr = liftIO $ FFI.getRangeEnd sr
