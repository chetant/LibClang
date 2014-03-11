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
import Clang.String (ClangString)

getUSR :: ClangBase m => FFI.Cursor s' -> ClangT s m (ClangString s)
getUSR = FFI.getCursorUSR

constructUSR_ObjCClass :: ClangBase m => String -> ClangT s m (ClangString s)
constructUSR_ObjCClass = FFI.constructUSR_ObjCClass

constructUSR_ObjCCategory :: ClangBase m => String -> String -> ClangT s m (ClangString s)
constructUSR_ObjCCategory = FFI.constructUSR_ObjCCategory

constructUSR_ObjCProtocol :: ClangBase m => String -> ClangT s m (ClangString s)
constructUSR_ObjCProtocol = FFI.constructUSR_ObjCProtocol

constructUSR_ObjCIvar :: ClangBase m => String -> ClangString s' -> ClangT s m (ClangString s)
constructUSR_ObjCIvar = FFI.constructUSR_ObjCIvar

constructUSR_ObjCMethod :: ClangBase m => String -> Bool -> ClangString s' -> ClangT s m (ClangString s)
constructUSR_ObjCMethod = FFI.constructUSR_ObjCMethod

constructUSR_ObjCProperty :: ClangBase m => String -> ClangString s' -> ClangT s m (ClangString s)
constructUSR_ObjCProperty = FFI.constructUSR_ObjCProperty
