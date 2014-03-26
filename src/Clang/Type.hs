{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clang.Type
( FFI.Type
, FFI.TypeKind(..)
, FFI.type_FirstBuiltin
, FFI.type_LastBuiltin
, FFI.CallingConv(..)
, FFI.CXXAccessSpecifier(..)
, FFI.TypeLayoutError(..)
, FFI.RefQualifierKind(..)

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
, getFieldDeclBitWidth

, isConstQualifiedType
, isVolatileQualifiedType
, isRestrictQualifiedType
, isPODType
, getElementType
, getNumElements
, getArrayElementType
, getArraySize
, getAlignOf
, getSizeOf
, getOffsetOf
, getClassType
, getCXXRefQualifier
, isVirtualBase
, getFunctionTypeCallingConv

, getTypeKindSpelling
) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.Word (Word64)

import qualified Clang.Internal.FFI as FFI
import Clang.Monad
import Clang.String (ClangString)

isSameType :: ClangBase m => FFI.Type s' -> FFI.Type s'' -> ClangT s m Bool
isSameType a b = liftIO $ FFI.equalTypes a b

getTypeSpelling :: ClangBase m => FFI.Type s' -> ClangT s m (ClangString s)
getTypeSpelling = FFI.getTypeSpelling

getTypedefDeclUnderlyingType :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Type s)
getTypedefDeclUnderlyingType c = liftIO $ FFI.getTypedefDeclUnderlyingType mkProxy c

getEnumDeclIntegerType :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Type s)
getEnumDeclIntegerType c = liftIO $ FFI.getEnumDeclIntegerType mkProxy c

getEnumConstantDeclValue :: ClangBase m => FFI.Cursor s' -> ClangT s m Int64
getEnumConstantDeclValue c = liftIO $ FFI.getEnumConstantDeclValue c

getEnumConstantDeclUnsignedValue :: ClangBase m => FFI.Cursor s' -> ClangT s m Word64
getEnumConstantDeclUnsignedValue c = liftIO $ FFI.getEnumConstantDeclUnsignedValue c

getFieldDeclBitWidth :: ClangBase m => FFI.Cursor s' -> ClangT s m (Maybe Int)
getFieldDeclBitWidth c = do
  bitWidth <- liftIO $ FFI.getFieldDeclBitWidth c
  return $ if bitWidth < 0 then Nothing
                           else Just bitWidth
    
getKind :: ClangBase m => FFI.Type s' -> ClangT s m FFI.TypeKind
getKind = return . FFI.getTypeKind

getCanonicalType :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Type s)
getCanonicalType t = liftIO $ FFI.getCanonicalType mkProxy t

getPointeeType :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Type s)
getPointeeType t = liftIO $ FFI.getPointeeType mkProxy t

getResultType :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Type s)
getResultType t = liftIO $ FFI.getResultType mkProxy t

getNumArgTypes :: ClangBase m => FFI.Type s' -> ClangT s m Int
getNumArgTypes t = liftIO $ FFI.getNumArgTypes t

getArgType :: ClangBase m => FFI.Type s' -> Int -> ClangT s m (FFI.Type s)
getArgType t i = liftIO $ FFI.getArgType mkProxy t i

isFunctionTypeVariadic :: ClangBase m => FFI.Type s' -> ClangT s m Bool
isFunctionTypeVariadic t = liftIO $ FFI.isFunctionTypeVariadic t

isConstQualifiedType :: ClangBase m => FFI.Type s' -> ClangT s m Bool
isConstQualifiedType t = liftIO $ FFI.isConstQualifiedType t

isVolatileQualifiedType :: ClangBase m => FFI.Type s' -> ClangT s m Bool
isVolatileQualifiedType t = liftIO $ FFI.isVolatileQualifiedType t

isRestrictQualifiedType :: ClangBase m => FFI.Type s' -> ClangT s m Bool
isRestrictQualifiedType t = liftIO $ FFI.isRestrictQualifiedType t

isPODType :: ClangBase m => FFI.Type s' -> ClangT s m Bool
isPODType t = liftIO $ FFI.isPODType t

getElementType :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Type s)
getElementType t = liftIO $ FFI.getElementType mkProxy t

getNumElements :: ClangBase m => FFI.Type s' -> ClangT s m Int64
getNumElements t = liftIO $ FFI.getNumElements t

getArrayElementType :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Type s)
getArrayElementType t = liftIO $ FFI.getArrayElementType mkProxy t

getArraySize :: ClangBase m => FFI.Type s' -> ClangT s m Int64
getArraySize t = liftIO $ FFI.getArraySize t

getAlignOf :: ClangBase m => FFI.Type s' -> ClangT s m (Either FFI.TypeLayoutError Int64)
getAlignOf t = liftIO $ FFI.type_getAlignOf t

getSizeOf :: ClangBase m => FFI.Type s' -> ClangT s m (Either FFI.TypeLayoutError Int64)
getSizeOf t = liftIO $ FFI.type_getSizeOf t

getOffsetOf :: ClangBase m => FFI.Type s' -> B.ByteString
            -> ClangT s m (Either FFI.TypeLayoutError Int64)
getOffsetOf = FFI.type_getOffsetOf

getClassType :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Type s)
getClassType t = liftIO $ FFI.type_getClassType mkProxy t

getCXXRefQualifier :: ClangBase m => FFI.Type s' -> ClangT s m FFI.RefQualifierKind
getCXXRefQualifier t = liftIO $ FFI.type_getCXXRefQualifier t

isVirtualBase :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c

getFunctionTypeCallingConv :: ClangBase m => FFI.Type s' -> ClangT s m FFI.CallingConv
getFunctionTypeCallingConv t = liftIO $ FFI.getFunctionTypeCallingConv t


-- Typekind functions
getTypeKindSpelling :: ClangBase m => FFI.TypeKind -> ClangT s m (ClangString s)
getTypeKindSpelling = FFI.getTypeKindSpelling
