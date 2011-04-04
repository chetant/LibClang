module Clang.CrossReference
(
 
) where

import qualified Clang.FFI as FFI
import Type
import Cursor

-- constructUSR_ObjCCategory
getUSR = unsafePerformIO . FFI.getCursorUSR
constructUSR_ObjCClass = FFI.constructCursorUSR_ObjCClass
constructUSR_ObjCCategory = FFI.constructCursorUSR_ObjCCategory cname catname)
constructUSR_ObjCProtocol = FFI.constructCursorUSR_ObjCProtocol
constructUSR_ObjCIvar = FFI.constructCursorUSR_ObjCIvar
constructUSR_ObjCMethod = FFI.constructCursorUSR_ObjCMethod
constructUSR_ObjCProperty = FFI.constructCursorUSR_ObjCProperty
