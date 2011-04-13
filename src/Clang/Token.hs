module Clang.Token
(
 FFI.Token
,Clang.Token.getKind
,getSpelling
,Clang.Token.getLocation
,getExtent
,tokenize
) where

import System.IO.Unsafe(unsafePerformIO)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

getKind = unsafePerformIO . FFI.getTokenKind
getSpelling t tk = unsafePerformIO (FFI.getTokenSpelling t tk)
getLocation t tk = unsafePerformIO (FFI.getTokenLocation t tk)
getExtent t tk = unsafePerformIO (FFI.getTokenExtent t tk)
tokenize :: FFI.TranslationUnit -> FFI.SourceRange -> [FFI.Token]
tokenize t sr = unsafePerformIO (FFI.tokenize t sr)
