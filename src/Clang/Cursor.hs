{-|
  The Cursor is an easy way to reference something in code and query its properties and relationships
  This is the primary way of traversing and querying code
-}
module Clang.Cursor
( FFI.CursorKind(..)
, FFI.LinkageKind(..)
, FFI.LanguageKind(..)
, FFI.Cursor
, FFI.CursorSet

, isSameCursor
, isNullCursor
, nullCursor
, getHash
, Clang.Cursor.getKind
, getLinkage
, getAvailability
, getLanguage
, getSemanticParent
, getLexicalParent
, getOverriddenCursors
, getIncludedFile
, Clang.Cursor.getLocation
, getExtent
, getType
, Clang.Cursor.getResultType
, getDeclObjCTypeEncoding
, getSpelling
, getDisplayName
, getReferenced
, getDefinition
, getCanonicalCursor
, getTemplateKind
, getSpecializedTemplate
, getTypeDeclaration
, getBaseExpression

-- attribute function
, getIBOutletCollectionType

, isDefinition
, isDeclaration
, isReference
, isExpression
, isStatement
, isInvalid
, isTranslationUnit
, isPreprocessing
, isUnexposed
, Clang.Cursor.isVirtualBase
, isPureVirtualCppMethod
, isStaticCppMethod
, isVirtualCppMethod
, isDynamicCall
, getCXXAccessSpecifier
, getOverloadedDecls

-- CursorSet functions
, createSet
, setContains
, setInsert

--CursorKind functions
, getCursorKindSpelling
) where

import Control.Monad.IO.Class
import GHC.Word

import qualified Clang.Internal.FFI as FFI
import Clang.Monad

isSameCursor :: FFI.Cursor -> FFI.Cursor -> ClangApp s Bool
isSameCursor a b = liftIO $ FFI.equalCursors a b

isNullCursor :: FFI.Cursor -> ClangApp s Bool
isNullCursor c = liftIO $ FFI.cursor_isNull c

nullCursor :: ClangApp s FFI.Cursor
nullCursor = liftIO FFI.getNullCursor

getHash :: FFI.Cursor -> ClangApp s GHC.Word.Word32
getHash c = liftIO $ FFI.hashCursor c

getKind :: FFI.Cursor -> ClangApp s FFI.CursorKind
getKind c = liftIO $ FFI.getCursorKind c

getLinkage :: FFI.Cursor -> ClangApp s FFI.LinkageKind
getLinkage c = liftIO $ FFI.getCursorLinkage c

getAvailability  :: FFI.Cursor -> ClangApp s FFI.AvailabilityKind
getAvailability c = liftIO $ FFI.getCursorAvailability c

getLanguage :: FFI.Cursor -> ClangApp s FFI.LanguageKind
getLanguage c = liftIO $ FFI.getCursorLanguage c

getSemanticParent :: FFI.Cursor -> ClangApp s FFI.Cursor
getSemanticParent c = liftIO $ FFI.getCursorSemanticParent c

getLexicalParent :: FFI.Cursor -> ClangApp s FFI.Cursor
getLexicalParent c = liftIO $ FFI.getCursorLexicalParent c

getOverriddenCursors :: FFI.Cursor -> ClangApp s [FFI.Cursor]
getOverriddenCursors c = liftIO $ FFI.getOverriddenCursors c

getIncludedFile :: FFI.Cursor -> ClangApp s FFI.File
getIncludedFile c = liftIO $ FFI.getIncludedFile c

getLocation :: FFI.Cursor -> ClangApp s FFI.SourceLocation
getLocation c = liftIO $ FFI.getCursorLocation c

getExtent :: FFI.Cursor -> ClangApp s FFI.SourceRange
getExtent c = liftIO $ FFI.getCursorExtent c

getType :: FFI.Cursor -> ClangApp s FFI.Type
getType c = liftIO $ FFI.getCursorType c

getResultType :: FFI.Cursor -> ClangApp s FFI.Type
getResultType c = liftIO $ FFI.getCursorResultType c

getDeclObjCTypeEncoding :: FFI.Cursor -> ClangApp s FFI.CXString
getDeclObjCTypeEncoding c = FFI.registerCXString $ FFI.getDeclObjCTypeEncoding c

getSpelling :: FFI.Cursor -> ClangApp s FFI.CXString
getSpelling c = FFI.registerCXString $ FFI.getCursorSpelling c

getDisplayName :: FFI.Cursor -> ClangApp s FFI.CXString
getDisplayName c = FFI.registerCXString $ FFI.getCursorDisplayName c

getReferenced :: FFI.Cursor -> ClangApp s FFI.Cursor
getReferenced c = liftIO $ FFI.getCursorReferenced c

getDefinition :: FFI.Cursor -> ClangApp s FFI.Cursor
getDefinition c = liftIO $ FFI.getCursorDefinition c

getCanonicalCursor :: FFI.Cursor -> ClangApp s FFI.Cursor
getCanonicalCursor c = liftIO $ FFI.getCanonicalCursor c

getTemplateKind :: FFI.Cursor -> ClangApp s FFI.CursorKind
getTemplateKind c = liftIO $ FFI.getTemplateCursorKind c

getSpecializedTemplate :: FFI.Cursor -> ClangApp s FFI.Cursor
getSpecializedTemplate c = liftIO $ FFI.getSpecializedCursorTemplate c

getTypeDeclaration :: FFI.Type -> ClangApp s FFI.Cursor
getTypeDeclaration t = liftIO $ FFI.getTypeDeclaration t

getBaseExpression :: FFI.Cursor -> ClangApp s FFI.Cursor
getBaseExpression c = liftIO $ FFI.cursor_getBaseExpression c

-- attribute function

getIBOutletCollectionType :: FFI.Cursor -> ClangApp s FFI.Type
getIBOutletCollectionType c = liftIO $ FFI.getIBOutletCollectionType c

isDefinition :: FFI.Cursor -> ClangApp s Bool
isDefinition c = liftIO $ FFI.isCursorDefinition c

isDeclaration :: FFI.CursorKind -> ClangApp s Bool
isDeclaration k = liftIO $ FFI.isDeclaration k

isReference :: FFI.CursorKind -> ClangApp s Bool
isReference k = liftIO $ FFI.isReference k

isExpression :: FFI.CursorKind -> ClangApp s Bool
isExpression k = liftIO $ FFI.isExpression k

isStatement :: FFI.CursorKind -> ClangApp s Bool
isStatement k = liftIO $ FFI.isStatement k

isInvalid :: FFI.CursorKind -> ClangApp s Bool
isInvalid k = liftIO $ FFI.isInvalid k

isTranslationUnit :: FFI.CursorKind -> ClangApp s Bool
isTranslationUnit k = liftIO $ FFI.isTranslationUnit k

isPreprocessing :: FFI.CursorKind -> ClangApp s Bool
isPreprocessing k = liftIO $ FFI.isPreprocessing k

isUnexposed :: FFI.CursorKind -> ClangApp s Bool
isUnexposed k = liftIO $ FFI.isUnexposed k

isVirtualBase :: FFI.Cursor -> ClangApp s Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c

isPureVirtualCppMethod :: FFI.Cursor -> ClangApp s Bool
isPureVirtualCppMethod c = liftIO $ FFI.cXXMethod_isPureVirtual c

isStaticCppMethod :: FFI.Cursor -> ClangApp s Bool
isStaticCppMethod c = liftIO $ FFI.cXXMethod_isStatic c

isVirtualCppMethod :: FFI.Cursor -> ClangApp s Bool
isVirtualCppMethod c = liftIO $ FFI.cXXMethod_isVirtual c

isDynamicCall :: FFI.Cursor -> ClangApp s Bool
isDynamicCall c = liftIO $ FFI.cursor_isDynamicCall c

getCXXAccessSpecifier :: FFI.Cursor -> ClangApp s FFI.CXXAccessSpecifier
getCXXAccessSpecifier c = liftIO $ FFI.getCXXAccessSpecifier c

getOverloadedDecls :: FFI.Cursor -> ClangApp s [FFI.Cursor]
getOverloadedDecls c = liftIO $ do
                         numDecls <- FFI.getNumOverloadedDecls c
                         mapM (FFI.getOverloadedDecl c) [0..(numDecls-1)]

-- CursorSet functions

createSet :: ClangApp s FFI.CursorSet
createSet = liftIO $ FFI.createCXCursorSet

setContains :: FFI.CursorSet -> FFI.Cursor -> ClangApp s Bool
setContains s c = liftIO $ FFI.cXCursorSet_contains s c

setInsert :: FFI.CursorSet -> FFI.Cursor -> ClangApp s Bool
setInsert s c = liftIO $ FFI.cXCursorSet_insert s c


--CursorKind functions

getCursorKindSpelling :: FFI.CursorKind -> ClangApp s FFI.CXString
getCursorKindSpelling k = FFI.registerCXString $ FFI.getCursorKindSpelling k
