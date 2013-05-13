module Clang.File
( FFI.UnsavedFile
, FFI.File
, getName
, getPOSIXTime
, getUTCTime
, getFile
) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getName :: FFI.File -> ClangApp s FFI.CXString
getName f = FFI.registerCXString $ FFI.getFileName f

getPOSIXTime :: FFI.File -> ClangApp s POSIXTime
getPOSIXTime f = liftIO $ realToFrac <$> FFI.getFileTime f

getUTCTime :: FFI.File -> ClangApp s UTCTime
getUTCTime f = liftIO $ posixSecondsToUTCTime . realToFrac <$> FFI.getFileTime f

getFile :: FFI.TranslationUnit -> FilePath -> ClangApp s FFI.File
getFile t f = liftIO $ FFI.getFile t f
