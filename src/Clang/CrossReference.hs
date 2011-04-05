module Clang.CrossReference
(
 
) where

import System.IO.Unsafe(unsafePerformIO)

import qualified Clang.FFI as FFI
import Clang.Type
import Clang.Cursor

-- constructUSR_ObjCCategory
getUSR = unsafePerformIO . FFI.getCursorUSR
constructUSR_ObjCClass = FFI.constructUSR_ObjCClass
constructUSR_ObjCCategory = FFI.constructUSR_ObjCCategory
constructUSR_ObjCProtocol = FFI.constructUSR_ObjCProtocol
constructUSR_ObjCIvar = FFI.constructUSR_ObjCIvar
constructUSR_ObjCMethod = FFI.constructUSR_ObjCMethod
constructUSR_ObjCProperty = FFI.constructUSR_ObjCProperty
