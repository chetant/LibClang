module Clang.File
(
 FFI.UnsavedFile
,FFI.File
,getName
,getTime
,getFile
) where

import Control.Monad((>=>))
import Control.Applicative((<$>))
import Data.Time.Clock.POSIX(POSIXTime, posixSecondsToUTCTime)
import System.IO.Unsafe(unsafePerformIO)
import qualified Clang.FFI as FFI

getName :: FFI.File -> FilePath
getName = unsafePerformIO . (FFI.getFileName >=> FFI.getCString)
getTime f = unsafePerformIO $ posixSecondsToUTCTime . realToFrac <$> FFI.getFileTime f
getFile :: FFI.TranslationUnit -> FilePath -> FFI.File
getFile t f = unsafePerformIO $ FFI.getFile t f
