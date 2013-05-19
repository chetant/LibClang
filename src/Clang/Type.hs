module Clang.Type
( FFI.Type
, FFI.TypeKind(..)
, FFI.CXXAccessSpecifier(..)

, isSameType
, getTypeSpelling
, getKind
, getCanonicalType
, getPointeeType
, getResultType
, getNumArgTypes
, getArgType
, isFunctionTypeVariadic

, getTypedefDeclUnderlyingType
, getEnumDeclIntegerType
, getEnumConstantDeclValue
, getEnumConstantDeclUnsignedValue

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

import Data.Int ( Int64 )
import Data.Word ( Word64 )

isSameType :: FFI.Type -> FFI.Type -> ClangApp s Bool
isSameType a b = liftIO $ FFI.equalTypes a b

getTypeSpelling :: FFI.Type -> ClangApp s FFI.CXString
getTypeSpelling t = FFI.registerCXString $ FFI.getTypeSpelling t

getTypedefDeclUnderlyingType :: FFI.Cursor -> ClangApp s FFI.Type
getTypedefDeclUnderlyingType c = liftIO $ FFI.getTypedefDeclUnderlyingType c

getEnumDeclIntegerType :: FFI.Cursor -> ClangApp s FFI.Type
getEnumDeclIntegerType c = liftIO $ FFI.getEnumDeclIntegerType c

getEnumConstantDeclValue :: FFI.Cursor -> ClangApp s Int64
getEnumConstantDeclValue c = liftIO $ FFI.getEnumConstantDeclValue c

getEnumConstantDeclUnsignedValue :: FFI.Cursor -> ClangApp s Word64
getEnumConstantDeclUnsignedValue c = liftIO $ FFI.getEnumConstantDeclUnsignedValue c

getKind :: FFI.Type -> ClangApp s FFI.TypeKind
getKind = return . FFI.getTypeKind

getCanonicalType :: FFI.Type -> ClangApp s FFI.Type
getCanonicalType t = liftIO $ FFI.getCanonicalType t

getPointeeType :: FFI.Type -> ClangApp s FFI.Type
getPointeeType t = liftIO $ FFI.getPointeeType t

getResultType :: FFI.Type -> ClangApp s FFI.Type
getResultType t = liftIO $ FFI.getResultType t

getNumArgTypes :: FFI.Type -> ClangApp s Int
getNumArgTypes t = liftIO $ FFI.getNumArgTypes t

getArgType :: FFI.Type -> Int -> ClangApp s FFI.Type
getArgType t i = liftIO $ FFI.getArgType t i

isFunctionTypeVariadic :: FFI.Type -> ClangApp s Bool
isFunctionTypeVariadic t = liftIO $ FFI.isFunctionTypeVariadic t

isConstQualifiedType :: FFI.Type -> ClangApp s Bool
isConstQualifiedType t = liftIO $ FFI.isConstQualifiedType t

isVolatileQualifiedType :: FFI.Type -> ClangApp s Bool
isVolatileQualifiedType t = liftIO $ FFI.isVolatileQualifiedType t

isRestrictQualifiedType :: FFI.Type -> ClangApp s Bool
isRestrictQualifiedType t = liftIO $ FFI.isRestrictQualifiedType t

isPODType :: FFI.Type -> ClangApp s Bool
isPODType t = liftIO $ FFI.isPODType t

isVirtualBase :: FFI.Cursor -> ClangApp s Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c


-- Typekind functions
getTypeKindSpelling :: FFI.TypeKind -> ClangApp s FFI.CXString
getTypeKindSpelling k = FFI.registerCXString $ FFI.getTypeKindSpelling k
