module Clang.File
(
 FFI.UnsavedFile
,FFI.File
,getFileName
,getFile
) where

import Control.Monad((>=>))
import System.IO.Unsafe(unsafePerformIO)
import qualified Clang.FFI as FFI

getFileName :: FFI.File -> FilePath
getFileName = unsafePerformIO . (FFI.getFileName >=> FFI.getCString)
getFileTime = unsafePerformIO . FFI.getFileTime
getFile :: FFI.TranslationUnit -> FilePath -> FFI.File
getFile t f = unsafePerformIO $ FFI.getFile t f
