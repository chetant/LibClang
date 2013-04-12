{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Clang.Internal.BaseFFI
( Index
, IndexPtr
, IndexObj(..)
, TranslationUnit
, TranslationUnitPtr
, TranslationUnitObj(..)
) where

import Foreign.Ptr
import Foreign.ForeignPtr

-- To avoid import cycles, we define a few of the most fundamental FFI types
-- separately from the rest of the FFI code.

-- typedef void *CXIndex;
data IndexObj
type Index = Ptr IndexObj
type IndexPtr = ForeignPtr IndexObj

-- typedef struct CXTranslationUnitImpl *CXTranslationUnit;
data TranslationUnitObj
type TranslationUnit = Ptr TranslationUnitObj
type TranslationUnitPtr = ForeignPtr TranslationUnitObj
