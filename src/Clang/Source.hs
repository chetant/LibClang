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
nullLocation :: ClangApp s FFI.SourceLocation
nullLocation = liftIO FFI.getNullLocation

isSameLocation :: FFI.SourceLocation -> FFI.SourceLocation -> ClangApp s Bool
isSameLocation a b = liftIO $ FFI.equalLocations a b

getLocation :: FFI.TranslationUnit -> FFI.File -> Int -> Int -> ClangApp s FFI.SourceLocation
getLocation tu f line col = liftIO $ FFI.getLocation tu f line col

getLocationForOffset :: FFI.TranslationUnit -> FFI.File -> Int -> ClangApp s FFI.SourceLocation
getLocationForOffset tu f off = liftIO $ FFI.getLocationForOffset tu f off

getCursor :: FFI.TranslationUnit -> FFI.SourceLocation -> ClangApp s FFI.Cursor
getCursor tu sl = liftIO $ FFI.getCursor tu sl

-- Range functions
nullRange :: ClangApp s FFI.SourceRange
nullRange = liftIO FFI.getNullRange

getRange :: FFI.SourceLocation -> FFI.SourceLocation -> ClangApp s FFI.SourceRange
getRange from to = liftIO $ FFI.getRange from to

getExpansionLocation :: FFI.SourceLocation -> ClangApp s (Maybe FFI.File, Int, Int, Int)
getExpansionLocation l = liftIO $ FFI.getExpansionLocation l

getInstantiationLocation :: FFI.SourceLocation -> ClangApp s (Maybe FFI.File, Int, Int, Int)
getInstantiationLocation l = liftIO $ FFI.getInstantiationLocation l

getSpellingLocation:: FFI.SourceLocation -> ClangApp s (Maybe FFI.File, Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getSpellingLocation l

getStart :: FFI.SourceRange -> ClangApp s FFI.SourceLocation
getStart r = liftIO $ FFI.getRangeStart r

getEnd :: FFI.SourceRange -> ClangApp s FFI.SourceLocation
getEnd r = liftIO $ FFI.getRangeEnd r
