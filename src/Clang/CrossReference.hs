module Clang.CrossReference
( getUSR
, constructUSR_ObjCClass
, constructUSR_ObjCCategory
, constructUSR_ObjCProtocol
, constructUSR_ObjCIvar
, constructUSR_ObjCMethod
, constructUSR_ObjCProperty
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

getUSR :: FFI.Cursor -> ClangApp FFI.CXString
getUSR c = liftIO $ FFI.getCursorUSR c

constructUSR_ObjCClass :: String -> ClangApp FFI.CXString
constructUSR_ObjCClass cls = liftIO $ FFI.constructUSR_ObjCClass cls

constructUSR_ObjCCategory :: String -> String -> ClangApp FFI.CXString
constructUSR_ObjCCategory cls cat = liftIO $ FFI.constructUSR_ObjCCategory cls cat

constructUSR_ObjCProtocol :: String -> ClangApp FFI.CXString
constructUSR_ObjCProtocol prt = liftIO $ FFI.constructUSR_ObjCProtocol prt

constructUSR_ObjCIvar :: String -> FFI.CXString -> ClangApp FFI.CXString
constructUSR_ObjCIvar v cls = liftIO $ FFI.constructUSR_ObjCIvar v cls

constructUSR_ObjCMethod :: String -> Bool -> FFI.CXString -> ClangApp FFI.CXString
constructUSR_ObjCMethod m isInstance cls = liftIO $ FFI.constructUSR_ObjCMethod m isInstance cls

constructUSR_ObjCProperty :: String -> FFI.CXString -> ClangApp FFI.CXString
constructUSR_ObjCProperty p cls = liftIO $ FFI.constructUSR_ObjCProperty p cls
