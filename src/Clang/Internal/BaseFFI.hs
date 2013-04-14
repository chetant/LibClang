{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Clang.Internal.BaseFFI
( Index (..)
, TranslationUnit (..)
) where

import Foreign.Ptr

-- To avoid import cycles, we define a couple of the most fundamental FFI types
-- separately from the rest of the FFI code.

-- typedef void *CXIndex;
data Index = Index (Ptr ())

-- typedef struct CXTranslationUnitImpl *CXTranslationUnit;
data TranslationUnit = TranslationUnit (Ptr ())
