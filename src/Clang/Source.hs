module Clang.Source
(
 FFI.File
,getFilename
,getFileTime
,FFI.SourceLocation
,nullLocation
,getLocation
,getLocationForOffset
,FFI.SourceRange
,nullRange
,getRange
,getInstantiationLocation
,getSpellingLocation
,getStart
,getEnd
) where

import System.IO.Unsafe(unsafePerformIO)

import qualified Clang.FFI as FFI

getFilename = unsafePerformIO . FFI.getFileName
getFileTime = unsafePerformIO . FFI.getFileTime


-- Location functions
nullLocation = unsafePerformIO FFI.getNullLocation

instance Eq FFI.SourceLocation where
    a == b = unsafePerformIO (FFI.equalLocations a b)

getLocation tu f line col = unsafePerformIO (FFI.getLocation tu f line col)
getLocationForOffset tu f off = unsafePerformIO (FFI.getLocationForOffset tu f off)


-- Range functions
nullRange = unsafePerformIO FFI.getNullRange
getRange from to = unsafePerformIO (FFI.getRange from to)
getInstantiationLocation = unsafePerformIO . FFI.getInstantiationLocation
getSpellingLocation = unsafePerformIO . FFI.getSpellingLocation
getStart = unsafePerformIO . FFI.getRangeStart
getEnd = unsafePerformIO . FFI.getRangeEnd
