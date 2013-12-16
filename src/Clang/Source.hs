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
nullLocation :: ClangBase m => ClangT s m FFI.SourceLocation
nullLocation = liftIO FFI.getNullLocation

isSameLocation :: ClangBase m => FFI.SourceLocation -> FFI.SourceLocation -> ClangT s m Bool
isSameLocation a b = liftIO $ FFI.equalLocations a b

getLocation :: ClangBase m => FFI.TranslationUnit -> FFI.File -> Int -> Int -> ClangT s m FFI.SourceLocation
getLocation tu f line col = liftIO $ FFI.getLocation tu f line col

getLocationForOffset :: ClangBase m => FFI.TranslationUnit -> FFI.File -> Int -> ClangT s m FFI.SourceLocation
getLocationForOffset tu f off = liftIO $ FFI.getLocationForOffset tu f off

getCursor :: ClangBase m => FFI.TranslationUnit -> FFI.SourceLocation -> ClangT s m FFI.Cursor
getCursor tu sl = liftIO $ FFI.getCursor tu sl

-- Range functions
nullRange :: ClangBase m => ClangT s m FFI.SourceRange
nullRange = liftIO FFI.getNullRange

getRange :: ClangBase m => FFI.SourceLocation -> FFI.SourceLocation -> ClangT s m FFI.SourceRange
getRange from to = liftIO $ FFI.getRange from to

getExpansionLocation :: ClangBase m => FFI.SourceLocation -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getExpansionLocation l = liftIO $ FFI.getExpansionLocation l

getInstantiationLocation :: ClangBase m => FFI.SourceLocation -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getInstantiationLocation l = liftIO $ FFI.getInstantiationLocation l

getSpellingLocation:: ClangBase m => FFI.SourceLocation -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getSpellingLocation l

getStart :: ClangBase m => FFI.SourceRange -> ClangT s m FFI.SourceLocation
getStart r = liftIO $ FFI.getRangeStart r

getEnd :: ClangBase m => FFI.SourceRange -> ClangT s m FFI.SourceLocation
getEnd r = liftIO $ FFI.getRangeEnd r
