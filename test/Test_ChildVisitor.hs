{-# LANGUAGE FlexibleContexts #-}
import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO)
import Text.Printf(printf)
import qualified Data.Vector.Storable as DVS(mapM_)

import Clang.String(unpack)
import Clang.TranslationUnit(getCursor)
import Clang(parseSourceFile, getChildren)
import Clang.Cursor(getKind, getDisplayName, getCursorKindSpelling)

test tu = getCursor tu >>= getChildren >>= DVS.mapM_ printInfo
  where
    printInfo c = do
      name <- getDisplayName c >>= unpack
      tstr <- getCursorKindSpelling (getKind c) >>= unpack
      liftIO $ printf "Name:%s, Kind:%s\n" name tstr

main = do
  (arg:args) <- getArgs
  parseSourceFile arg args test
