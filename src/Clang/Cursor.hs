{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
, FFI.ParentedCursor(..)

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
, Clang.Cursor.getSpellingLocation
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
, getNumArguments
, getArgument

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

isSameCursor :: ClangBase m => FFI.Cursor -> FFI.Cursor -> ClangT s m Bool
isSameCursor a b = liftIO $ FFI.equalCursors a b

isNullCursor :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isNullCursor c = liftIO $ FFI.cursor_isNull c

nullCursor :: ClangBase m => ClangT s m FFI.Cursor
nullCursor = liftIO FFI.getNullCursor

getHash :: ClangBase m => FFI.Cursor -> ClangT s m GHC.Word.Word32
getHash c = liftIO $ FFI.hashCursor c

getKind :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CursorKind
getKind c = liftIO $ FFI.getCursorKind c

getLinkage :: ClangBase m => FFI.Cursor -> ClangT s m FFI.LinkageKind
getLinkage c = liftIO $ FFI.getCursorLinkage c

getAvailability  :: ClangBase m => FFI.Cursor -> ClangT s m FFI.AvailabilityKind
getAvailability c = liftIO $ FFI.getCursorAvailability c

getLanguage :: ClangBase m => FFI.Cursor -> ClangT s m FFI.LanguageKind
getLanguage c = liftIO $ FFI.getCursorLanguage c

getSemanticParent :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getSemanticParent c = liftIO $ FFI.getCursorSemanticParent c

getLexicalParent :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getLexicalParent c = liftIO $ FFI.getCursorLexicalParent c

getOverriddenCursors :: ClangBase m => FFI.Cursor -> ClangT s m [FFI.Cursor]
getOverriddenCursors c = liftIO $ FFI.getOverriddenCursors c

getIncludedFile :: ClangBase m => FFI.Cursor -> ClangT s m FFI.File
getIncludedFile c = liftIO $ FFI.getIncludedFile c

getLocation :: ClangBase m => FFI.Cursor -> ClangT s m FFI.SourceLocation
getLocation c = liftIO $ FFI.getCursorLocation c

getSpellingLocation:: ClangBase m => FFI.Cursor -> ClangT s m (Maybe FFI.File, Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getCursorSpellingLocation l

getExtent :: ClangBase m => FFI.Cursor -> ClangT s m FFI.SourceRange
getExtent c = liftIO $ FFI.getCursorExtent c

getType :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Type
getType c = liftIO $ FFI.getCursorType c

getResultType :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Type
getResultType c = liftIO $ FFI.getCursorResultType c

getDeclObjCTypeEncoding :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CXString
getDeclObjCTypeEncoding c = FFI.registerCXString $ FFI.getDeclObjCTypeEncoding c

getSpelling :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CXString
getSpelling c = FFI.registerCXString $ FFI.getCursorSpelling c

getDisplayName :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CXString
getDisplayName c = FFI.registerCXString $ FFI.getCursorDisplayName c

getReferenced :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getReferenced c = liftIO $ FFI.getCursorReferenced c

getDefinition :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getDefinition c = liftIO $ FFI.getCursorDefinition c

getCanonicalCursor :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getCanonicalCursor c = liftIO $ FFI.getCanonicalCursor c

getTemplateKind :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CursorKind
getTemplateKind c = liftIO $ FFI.getTemplateCursorKind c

getSpecializedTemplate :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getSpecializedTemplate c = liftIO $ FFI.getSpecializedCursorTemplate c

getTypeDeclaration :: ClangBase m => FFI.Type -> ClangT s m FFI.Cursor
getTypeDeclaration t = liftIO $ FFI.getTypeDeclaration t

getBaseExpression :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Cursor
getBaseExpression c = liftIO $ FFI.cursor_getBaseExpression c

getNumArguments :: ClangBase m => FFI.Cursor -> ClangT s m Int
getNumArguments c = liftIO $ FFI.cursor_getNumArguments c

getArgument :: ClangBase m => FFI.Cursor -> Int -> ClangT s m FFI.Cursor
getArgument c i = liftIO $ FFI.cursor_getArgument c i

-- attribute function

getIBOutletCollectionType :: ClangBase m => FFI.Cursor -> ClangT s m FFI.Type
getIBOutletCollectionType c = liftIO $ FFI.getIBOutletCollectionType c

isDefinition :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isDefinition c = liftIO $ FFI.isCursorDefinition c

isDeclaration :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isDeclaration k = liftIO $ FFI.isDeclaration k

isReference :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isReference k = liftIO $ FFI.isReference k

isExpression :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isExpression k = liftIO $ FFI.isExpression k

isStatement :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isStatement k = liftIO $ FFI.isStatement k

isInvalid :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isInvalid k = liftIO $ FFI.isInvalid k

isTranslationUnit :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isTranslationUnit k = liftIO $ FFI.isTranslationUnit k

isPreprocessing :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isPreprocessing k = liftIO $ FFI.isPreprocessing k

isUnexposed :: ClangBase m => FFI.CursorKind -> ClangT s m Bool
isUnexposed k = liftIO $ FFI.isUnexposed k

isVirtualBase :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c

isPureVirtualCppMethod :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isPureVirtualCppMethod c = liftIO $ FFI.cXXMethod_isPureVirtual c

isStaticCppMethod :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isStaticCppMethod c = liftIO $ FFI.cXXMethod_isStatic c

isVirtualCppMethod :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isVirtualCppMethod c = liftIO $ FFI.cXXMethod_isVirtual c

isDynamicCall :: ClangBase m => FFI.Cursor -> ClangT s m Bool
isDynamicCall c = liftIO $ FFI.cursor_isDynamicCall c

getCXXAccessSpecifier :: ClangBase m => FFI.Cursor -> ClangT s m FFI.CXXAccessSpecifier
getCXXAccessSpecifier c = liftIO $ FFI.getCXXAccessSpecifier c

getOverloadedDecls :: ClangBase m => FFI.Cursor -> ClangT s m [FFI.Cursor]
getOverloadedDecls c = liftIO $ do
                         numDecls <- FFI.getNumOverloadedDecls c
                         mapM (FFI.getOverloadedDecl c) [0..(numDecls-1)]

-- CursorSet functions

createSet :: ClangBase m => ClangT s m FFI.CursorSet
createSet = liftIO $ FFI.createCXCursorSet

setContains :: ClangBase m => FFI.CursorSet -> FFI.Cursor -> ClangT s m Bool
setContains s c = liftIO $ FFI.cXCursorSet_contains s c

setInsert :: ClangBase m => FFI.CursorSet -> FFI.Cursor -> ClangT s m Bool
setInsert s c = liftIO $ FFI.cXCursorSet_insert s c


--CursorKind functions

getCursorKindSpelling :: ClangBase m => FFI.CursorKind -> ClangT s m FFI.CXString
getCursorKindSpelling k = FFI.registerCXString $ FFI.getCursorKindSpelling k
