{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Source
( FFI.SourceLocation
, nullLocation
, isSameLocation
, getLocation
, getLocationForOffset
, isInSystemHeader
, isFromMainFile
, getCursor
, FFI.SourceRange
, nullRange
, getRange
, isSameRange
, isNullRange
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
nullLocation = liftIO $ FFI.getNullLocation mkProxy

isSameLocation :: ClangBase m => FFI.SourceLocation s' -> FFI.SourceLocation s''
               -> ClangT s m Bool
isSameLocation a b = liftIO $ FFI.equalLocations a b

getLocation :: ClangBase m => FFI.TranslationUnit s' -> FFI.File s'' -> Int -> Int
            -> ClangT s m (FFI.SourceLocation s)
getLocation tu f line col = liftIO $ FFI.getLocation mkProxy tu f line col

getLocationForOffset :: ClangBase m => FFI.TranslationUnit s' -> FFI.File s'' -> Int
                     -> ClangT s m (FFI.SourceLocation s)
getLocationForOffset tu f off = liftIO $ FFI.getLocationForOffset mkProxy tu f off

isInSystemHeader :: ClangBase m => FFI.SourceLocation s' -> ClangT s m Bool
isInSystemHeader l = liftIO $ FFI.location_isInSystemHeader l

isFromMainFile :: ClangBase m => FFI.SourceLocation s' -> ClangT s m Bool
isFromMainFile l = liftIO $ FFI.location_isFromMainFile l

getCursor :: ClangBase m => FFI.TranslationUnit s' -> FFI.SourceLocation s''
          -> ClangT s m (FFI.Cursor s)
getCursor tu sl = liftIO $ FFI.getCursor mkProxy tu sl

-- Range functions
nullRange :: ClangBase m => ClangT s m (FFI.SourceRange s)
nullRange = liftIO $ FFI.getNullRange mkProxy

getRange :: ClangBase m => FFI.SourceLocation s' -> FFI.SourceLocation s''
         -> ClangT s m (FFI.SourceRange s)
getRange from to = liftIO $ FFI.getRange mkProxy from to

isSameRange :: ClangBase m => FFI.SourceRange s' -> FFI.SourceRange s'' -> ClangT s m Bool
isSameRange a b = liftIO $ FFI.equalRanges a b

isNullRange :: ClangBase m => FFI.SourceRange s' -> ClangT s m Bool
isNullRange r = liftIO $ FFI.range_isNull r

getExpansionLocation :: ClangBase m => FFI.SourceLocation s'
                     -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getExpansionLocation l = liftIO $ FFI.getExpansionLocation mkProxy l

getInstantiationLocation :: ClangBase m => FFI.SourceLocation s'
                         -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getInstantiationLocation l = liftIO $ FFI.getInstantiationLocation mkProxy l

getSpellingLocation :: ClangBase m => FFI.SourceLocation s'
                    -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getSpellingLocation mkProxy l

getStart :: ClangBase m => FFI.SourceRange s' -> ClangT s m (FFI.SourceLocation s)
getStart sr = liftIO $ FFI.getRangeStart mkProxy sr

getEnd :: ClangBase m => FFI.SourceRange s' -> ClangT s m (FFI.SourceLocation s)
getEnd sr = liftIO $ FFI.getRangeEnd mkProxy sr
