{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

isSameType :: ClangBase m => FFI.Type -> FFI.Type -> ClangT s m Bool
isSameType a b = liftIO $ FFI.equalTypes a b

getTypeSpelling :: ClangBase m => FFI.Type -> ClangT s m FFI.CXString
getTypeSpelling t = FFI.registerCXString $ FFI.getTypeSpelling t

getTypedefDeclUnderlyingType :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Type
getTypedefDeclUnderlyingType c = liftIO $ FFI.getTypedefDeclUnderlyingType c

getEnumDeclIntegerType :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Type
getEnumDeclIntegerType c = liftIO $ FFI.getEnumDeclIntegerType c

getEnumConstantDeclValue :: ClangBase m => FFI.Cursor -> ClangT s m Int64
getEnumConstantDeclValue c = liftIO $ FFI.getEnumConstantDeclValue c

getEnumConstantDeclUnsignedValue :: ClangBase m => FFI.Cursor -> ClangT s m Word64
getEnumConstantDeclUnsignedValue c = liftIO $ FFI.getEnumConstantDeclUnsignedValue c

getKind :: ClangBase m => FFI.Type -> ClangT s m FFI.TypeKind
getKind = return . FFI.getTypeKind

getCanonicalType :: ClangBase m => FFI.Type -> ClangT s m FFI.Type
getCanonicalType t = liftIO $ FFI.getCanonicalType t

getPointeeType :: ClangBase m => FFI.Type -> ClangT s m FFI.Type
getPointeeType t = liftIO $ FFI.getPointeeType t

getResultType :: ClangBase m => FFI.Type -> ClangT s m FFI.Type
getResultType t = liftIO $ FFI.getResultType t

getNumArgTypes :: ClangBase m => FFI.Type -> ClangT s m Int
getNumArgTypes t = liftIO $ FFI.getNumArgTypes t

getArgType :: ClangBase m => FFI.Type -> Int -> ClangT s m FFI.Type
getArgType t i = liftIO $ FFI.getArgType t i

isFunctionTypeVariadic :: ClangBase m => FFI.Type -> ClangT s m Bool
isFunctionTypeVariadic t = liftIO $ FFI.isFunctionTypeVariadic t

isConstQualifiedType :: ClangBase m => FFI.Type -> ClangT s m Bool
isConstQualifiedType t = liftIO $ FFI.isConstQualifiedType t

isVolatileQualifiedType :: ClangBase m => FFI.Type -> ClangT s m Bool
isVolatileQualifiedType t = liftIO $ FFI.isVolatileQualifiedType t

isRestrictQualifiedType :: ClangBase m => FFI.Type -> ClangT s m Bool
isRestrictQualifiedType t = liftIO $ FFI.isRestrictQualifiedType t

isPODType :: ClangBase m => FFI.Type -> ClangT s m Bool
isPODType t = liftIO $ FFI.isPODType t

isVirtualBase :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c


-- Typekind functions
getTypeKindSpelling :: ClangBase m => FFI.TypeKind -> ClangT s m FFI.CXString
getTypeKindSpelling k = FFI.registerCXString $ FFI.getTypeKindSpelling k
