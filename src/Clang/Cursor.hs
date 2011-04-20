{-|
  The Cursor is an easy way to reference something in code and query its properties and relationships
  This is the primary way of traversing and querying code
-}
module Clang.Cursor
(
 FFI.CursorKind(..)
,FFI.LinkageKind(..)
,FFI.LanguageKind(..)
,FFI.Cursor
,FFI.CursorSet

,nullCursor
,getHash
,Clang.Cursor.getKind
,getLinkage
,getAvailability
,getLanguage
,getSemanticParent
,getLexicalParent
,getOverriddenCursors
,getIncludedFile
,Clang.Cursor.getLocation
,getExtent
,getType
,Clang.Cursor.getResultType
,getDeclObjCTypeEncoding
,getSpelling
,getDisplayName
,getReferenced
,getDefinition
,getCanonicalCursor
,getTemplateKind
,getSpecializedTemplate
,getTypeDeclaration

-- attribute function
,getIBOutletCollectionType

,isDefinition
,isDeclaration
,isReference
,isExpression
,isStatement
,isInvalid
,isTranslationUnit
,isPreprocessing
,isUnexposed
,Clang.Cursor.isVirtualBase
,isStaticCppMethod

-- CursorSet functions
,createSet
,setContains
,setInsert

,getCXXAccessSpecifier
,getOverloadedDecls

--CursorKind functions
,getCursorKindSpelling
) where

import System.IO.Unsafe(unsafePerformIO)

import Clang.Type
import Clang.Source
import qualified Clang.FFI as FFI

instance Eq FFI.Cursor where
    a == b = unsafePerformIO (FFI.equalCursors a b)

nullCursor = unsafePerformIO FFI.getNullCursor
getHash = unsafePerformIO . FFI.hashCursor
getKind = unsafePerformIO . FFI.getCursorKind
getLinkage = unsafePerformIO . FFI.getCursorLinkage
getAvailability = unsafePerformIO . FFI.getCursorAvailability
getLanguage = unsafePerformIO . FFI.getCursorLanguage
getSemanticParent = unsafePerformIO . FFI.getCursorSemanticParent
getLexicalParent = unsafePerformIO . FFI.getCursorLexicalParent
getOverriddenCursors :: FFI.Cursor -> [FFI.Cursor]
getOverriddenCursors = unsafePerformIO . FFI.getOverriddenCursors
getIncludedFile = unsafePerformIO . FFI.getIncludedFile
getLocation = unsafePerformIO . FFI.getCursorLocation
getExtent = unsafePerformIO . FFI.getCursorExtent
getType = unsafePerformIO . FFI.getCursorType
getResultType = unsafePerformIO . FFI.getCursorResultType
getDeclObjCTypeEncoding = unsafePerformIO . FFI.getDeclObjCTypeEncoding
getSpelling = unsafePerformIO . FFI.getCursorSpelling
getDisplayName = unsafePerformIO . FFI.getCursorDisplayName
getReferenced = unsafePerformIO . FFI.getCursorReferenced
getDefinition = unsafePerformIO . FFI.getCursorDefinition
getCanonicalCursor = unsafePerformIO . FFI.getCanonicalCursor
getTemplateKind = unsafePerformIO . FFI.getTemplateCursorKind
getSpecializedTemplate = unsafePerformIO . FFI.getSpecializedCursorTemplate
getTypeDeclaration = unsafePerformIO . FFI.getTypeDeclaration

-- attribute function
getIBOutletCollectionType = unsafePerformIO . FFI.getIBOutletCollectionType

isDefinition = unsafePerformIO . FFI.isCursorDefinition
isDeclaration = unsafePerformIO . FFI.isDeclaration
isReference = unsafePerformIO . FFI.isReference
isExpression = unsafePerformIO . FFI.isExpression
isStatement = unsafePerformIO . FFI.isStatement
isInvalid = unsafePerformIO . FFI.isInvalid
isTranslationUnit = unsafePerformIO . FFI.isTranslationUnit
isPreprocessing = unsafePerformIO . FFI.isPreprocessing
isUnexposed = unsafePerformIO . FFI.isUnexposed
isVirtualBase = unsafePerformIO . FFI.isVirtualBase
isStaticCppMethod = unsafePerformIO . FFI.cXXMethod_isStatic

-- CursorSet functions
createSet = FFI.createCXCursorSet
setContains = FFI.cXCursorSet_contains
setInsert = FFI.cXCursorSet_insert

getCXXAccessSpecifier = unsafePerformIO . FFI.getCXXAccessSpecifier
getOverloadedDecls c = unsafePerformIO $ do
                         numDecls <- FFI.getNumOverloadedDecls c
                         mapM (FFI.getOverloadedDecl c) [0..(numDecls-1)]

--CursorKind functions
getCursorKindSpelling = unsafePerformIO . FFI.getCursorKindSpelling
