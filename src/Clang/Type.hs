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
import Data.Int (Int64)
import Data.Word (Word64)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

isSameType :: ClangBase m => FFI.Type s -> FFI.Type s -> ClangT s m Bool
isSameType a b = liftIO $ FFI.equalTypes a b

getTypeSpelling :: ClangBase m => FFI.Type s -> ClangT s m (ClangString s)
getTypeSpelling = FFI.getTypeSpelling

getTypedefDeclUnderlyingType :: ClangBase m => FFI.Cursor s -> ClangT s m (FFI.Type s)
getTypedefDeclUnderlyingType c = liftIO $ FFI.getTypedefDeclUnderlyingType c

getEnumDeclIntegerType :: ClangBase m => FFI.Cursor s -> ClangT s m (FFI.Type s)
getEnumDeclIntegerType c = liftIO $ FFI.getEnumDeclIntegerType c

getEnumConstantDeclValue :: ClangBase m => FFI.Cursor s -> ClangT s m Int64
getEnumConstantDeclValue c = liftIO $ FFI.getEnumConstantDeclValue c

getEnumConstantDeclUnsignedValue :: ClangBase m => FFI.Cursor s -> ClangT s m Word64
getEnumConstantDeclUnsignedValue c = liftIO $ FFI.getEnumConstantDeclUnsignedValue c

getKind :: ClangBase m => FFI.Type s -> ClangT s m FFI.TypeKind
getKind = return . FFI.getTypeKind

getCanonicalType :: ClangBase m => FFI.Type s -> ClangT s m (FFI.Type s)
getCanonicalType t = liftIO $ FFI.getCanonicalType t

getPointeeType :: ClangBase m => FFI.Type s -> ClangT s m (FFI.Type s)
getPointeeType t = liftIO $ FFI.getPointeeType t

getResultType :: ClangBase m => FFI.Type s -> ClangT s m (FFI.Type s)
getResultType t = liftIO $ FFI.getResultType t

getNumArgTypes :: ClangBase m => FFI.Type s -> ClangT s m Int
getNumArgTypes t = liftIO $ FFI.getNumArgTypes t

getArgType :: ClangBase m => FFI.Type s -> Int -> ClangT s m (FFI.Type s)
getArgType t i = liftIO $ FFI.getArgType t i

isFunctionTypeVariadic :: ClangBase m => FFI.Type s -> ClangT s m Bool
isFunctionTypeVariadic t = liftIO $ FFI.isFunctionTypeVariadic t

isConstQualifiedType :: ClangBase m => FFI.Type s -> ClangT s m Bool
isConstQualifiedType t = liftIO $ FFI.isConstQualifiedType t

isVolatileQualifiedType :: ClangBase m => FFI.Type s -> ClangT s m Bool
isVolatileQualifiedType t = liftIO $ FFI.isVolatileQualifiedType t

isRestrictQualifiedType :: ClangBase m => FFI.Type s -> ClangT s m Bool
isRestrictQualifiedType t = liftIO $ FFI.isRestrictQualifiedType t

isPODType :: ClangBase m => FFI.Type s -> ClangT s m Bool
isPODType t = liftIO $ FFI.isPODType t

isVirtualBase :: ClangBase m => FFI.Cursor s -> ClangT s m Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c


-- Typekind functions
getTypeKindSpelling :: ClangBase m => FFI.TypeKind -> ClangT s m (ClangString s)
getTypeKindSpelling = FFI.getTypeKindSpelling
