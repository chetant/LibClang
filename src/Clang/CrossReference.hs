{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.CrossReference
( getUSR
, constructUSR_ObjCClass
, constructUSR_ObjCCategory
, constructUSR_ObjCProtocol
, constructUSR_ObjCIvar
, constructUSR_ObjCMethod
, constructUSR_ObjCProperty
) where

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getUSR :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CXString
getUSR c = FFI.registerCXString $ FFI.getCursorUSR c

constructUSR_ObjCClass :: ClangBase m => String -> ClangT s m FFI.CXString
constructUSR_ObjCClass cls = FFI.registerCXString $ FFI.constructUSR_ObjCClass cls

constructUSR_ObjCCategory :: ClangBase m => String -> String -> ClangT s m FFI.CXString
constructUSR_ObjCCategory cls cat = FFI.registerCXString $ FFI.constructUSR_ObjCCategory cls cat

constructUSR_ObjCProtocol :: ClangBase m => String -> ClangT s m FFI.CXString
constructUSR_ObjCProtocol prt = FFI.registerCXString $ FFI.constructUSR_ObjCProtocol prt

constructUSR_ObjCIvar :: ClangBase m => String -> FFI.CXString -> ClangT s m FFI.CXString
constructUSR_ObjCIvar v cls = FFI.registerCXString $ FFI.constructUSR_ObjCIvar v cls

constructUSR_ObjCMethod :: ClangBase m => String -> Bool -> FFI.CXString -> ClangT s m FFI.CXString
constructUSR_ObjCMethod m isInstance cls = FFI.registerCXString $ FFI.constructUSR_ObjCMethod m isInstance cls

constructUSR_ObjCProperty :: ClangBase m => String -> FFI.CXString -> ClangT s m FFI.CXString
constructUSR_ObjCProperty p cls = FFI.registerCXString $ FFI.constructUSR_ObjCProperty p cls
