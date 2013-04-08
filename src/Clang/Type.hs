module Clang.Type
( FFI.Type
, FFI.TypeKind(..)
, FFI.CXXAccessSpecifier(..)

, isSameType
, getKind
, getCanonicalType
, getPointeeType
, getResultType

, isConstQualifiedType
, isVolatileQualifiedType
, isRestrictQualifiedType
, isPODType
, isVirtualBase
, getTypeKindSpelling
) where

import Control.Monad.IO.Class

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

isSameType :: FFI.Type -> FFI.Type -> ClangApp Bool
isSameType a b = liftIO $ FFI.equalTypes a b

getKind :: FFI.Type -> ClangApp FFI.TypeKind
getKind = return . FFI.getTypeKind

getCanonicalType :: FFI.Type -> ClangApp FFI.Type
getCanonicalType t = liftIO $ FFI.getCanonicalType t

getPointeeType :: FFI.Type -> ClangApp FFI.Type
getPointeeType t = liftIO $ FFI.getPointeeType t

getResultType :: FFI.Type -> ClangApp FFI.Type
getResultType t = liftIO $ FFI.getResultType t

isConstQualifiedType :: FFI.Type -> ClangApp Bool
isConstQualifiedType t = liftIO $ FFI.isConstQualifiedType t

isVolatileQualifiedType :: FFI.Type -> ClangApp Bool
isVolatileQualifiedType t = liftIO $ FFI.isVolatileQualifiedType t

isRestrictQualifiedType :: FFI.Type -> ClangApp Bool
isRestrictQualifiedType t = liftIO $ FFI.isRestrictQualifiedType t

isPODType :: FFI.Type -> ClangApp Bool
isPODType t = liftIO $ FFI.isPODType t

isVirtualBase :: FFI.Cursor -> ClangApp Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c


-- Typekind functions
getTypeKindSpelling :: FFI.TypeKind -> ClangApp FFI.CXString
getTypeKindSpelling k = liftIO $ FFI.getTypeKindSpelling k
