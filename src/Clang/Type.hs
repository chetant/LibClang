module Clang.Type
(
 FFI.Type
,FFI.CXString
,FFI.TypeKind(..)
,FFI.CXXAccessSpecifier(..)

,getKind
,getCanonicalType
,getPointeeType
,getResultType

,isConstQualifiedType
,isVolatileQualifiedType
,isRestrictQualifiedType
,isPODType
,isVirtualBase
)
where

import System.IO.Unsafe(unsafePerformIO)
import qualified Clang.FFI as FFI

instance Show FFI.CXString where
    show = unsafePerformIO . FFI.getCString

instance Eq FFI.Type where
    a == b = unsafePerformIO (FFI.equalTypes a b)

getKind = FFI.getTypeKind

getCanonicalType = unsafePerformIO . FFI.getCanonicalType
getPointeeType = unsafePerformIO . FFI.getPointeeType
getResultType = unsafePerformIO . FFI.getResultType

isConstQualifiedType = unsafePerformIO . FFI.isConstQualifiedType
isVolatileQualifiedType = unsafePerformIO . FFI.isVolatileQualifiedType
isRestrictQualifiedType = unsafePerformIO . FFI.isRestrictQualifiedType
isPODType = unsafePerformIO . FFI.isPODType
isVirtualBase = unsafePerformIO . FFI.isVirtualBase


-- Typekind functions
getTypeKindSpelling  = unsafePerformIO . FFI.getTypeKindSpelling
