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
import Clang.String (ClangString)

isNullCursor :: FFI.Cursor s -> Bool
isNullCursor = FFI.cursor_isNull
{-# INLINE isNullCursor #-}

nullCursor :: ClangBase m => ClangT s m (FFI.Cursor s)
nullCursor = FFI.getNullCursor
{-# INLINE nullCursor #-}

getHash :: ClangBase m => FFI.Cursor s' -> ClangT s m GHC.Word.Word32
getHash c = liftIO $ FFI.hashCursor c

getKind :: FFI.Cursor s -> FFI.CursorKind
getKind = FFI.getCursorKind
{-# INLINE getKind #-}

getLinkage :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.LinkageKind
getLinkage c = liftIO $ FFI.getCursorLinkage c

getAvailability  :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.AvailabilityKind
getAvailability c = liftIO $ FFI.getCursorAvailability c

getLanguage :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.LanguageKind
getLanguage c = liftIO $ FFI.getCursorLanguage c

getSemanticParent :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getSemanticParent c = liftIO $ FFI.getCursorSemanticParent mkProxy c

getLexicalParent :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getLexicalParent c = liftIO $ FFI.getCursorLexicalParent mkProxy c

getOverriddenCursors :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CursorList s)
getOverriddenCursors = FFI.getOverriddenCursors

getIncludedFile :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.File s)
getIncludedFile c = liftIO $ FFI.getIncludedFile mkProxy c

getLocation :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.SourceLocation s)
getLocation c = liftIO $ FFI.getCursorLocation mkProxy c

getSpellingLocation :: ClangBase m => FFI.Cursor s'
                    -> ClangT s m (Maybe (FFI.File s), Int, Int, Int)
getSpellingLocation l = liftIO $ FFI.getCursorSpellingLocation mkProxy l

getExtent :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.SourceRange s)
getExtent c = liftIO $ FFI.getCursorExtent mkProxy c

getType :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Type s)
getType c = liftIO $ FFI.getCursorType mkProxy c

getResultType :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Type s)
getResultType c = liftIO $ FFI.getCursorResultType mkProxy c

getDeclObjCTypeEncoding :: ClangBase m => FFI.Cursor s' -> ClangT s m (ClangString s)
getDeclObjCTypeEncoding = FFI.getDeclObjCTypeEncoding

getSpelling :: ClangBase m => FFI.Cursor s' -> ClangT s m (ClangString s)
getSpelling = FFI.getCursorSpelling

getDisplayName :: ClangBase m => FFI.Cursor s' -> ClangT s m (ClangString s)
getDisplayName = FFI.getCursorDisplayName

getReferenced :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getReferenced c = liftIO $ FFI.getCursorReferenced mkProxy c

getDefinition :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getDefinition c = liftIO $ FFI.getCursorDefinition mkProxy c

getCanonicalCursor :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getCanonicalCursor c = liftIO $ FFI.getCanonicalCursor mkProxy c

getTemplateKind :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.CursorKind
getTemplateKind c = liftIO $ FFI.getTemplateCursorKind c

getSpecializedTemplate :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getSpecializedTemplate c = liftIO $ FFI.getSpecializedCursorTemplate mkProxy c

getTypeDeclaration :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Cursor s)
getTypeDeclaration t = liftIO $ FFI.getTypeDeclaration mkProxy t

getBaseExpression :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getBaseExpression c = liftIO $ FFI.cursor_getBaseExpression mkProxy c

getNumArguments :: ClangBase m => FFI.Cursor s' -> ClangT s m Int
getNumArguments c = liftIO $ FFI.cursor_getNumArguments c

getArgument :: ClangBase m => FFI.Cursor s' -> Int -> ClangT s m (FFI.Cursor s)
getArgument c i = liftIO $ FFI.cursor_getArgument mkProxy c i

-- attribute function

getIBOutletCollectionType :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Type s)
getIBOutletCollectionType c = liftIO $ FFI.getIBOutletCollectionType mkProxy c

isDefinition :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isDefinition c = liftIO $ FFI.isCursorDefinition c

isDeclaration :: FFI.CursorKind -> Bool
isDeclaration = FFI.isDeclaration
{-# INLINE isDeclaration #-}

isReference :: FFI.CursorKind -> Bool
isReference = FFI.isReference
{-# INLINE isReference #-}

isExpression :: FFI.CursorKind -> Bool
isExpression = FFI.isExpression
{-# INLINE isExpression #-}

isStatement :: FFI.CursorKind -> Bool
isStatement = FFI.isStatement
{-# INLINE isStatement #-}

isInvalid :: FFI.CursorKind -> Bool
isInvalid = FFI.isInvalid
{-# INLINE isInvalid #-}

isTranslationUnit :: FFI.CursorKind -> Bool
isTranslationUnit = FFI.isTranslationUnit
{-# INLINE isTranslationUnit #-}

isPreprocessing :: FFI.CursorKind -> Bool
isPreprocessing = FFI.isPreprocessing
{-# INLINE isPreprocessing #-}

isUnexposed :: FFI.CursorKind -> Bool
isUnexposed = FFI.isUnexposed
{-# INLINE isUnexposed #-}

isVirtualBase :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isVirtualBase c = liftIO $ FFI.isVirtualBase c

isPureVirtualCppMethod :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isPureVirtualCppMethod c = liftIO $ FFI.cXXMethod_isPureVirtual c

isStaticCppMethod :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isStaticCppMethod c = liftIO $ FFI.cXXMethod_isStatic c

isVirtualCppMethod :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isVirtualCppMethod c = liftIO $ FFI.cXXMethod_isVirtual c

isDynamicCall :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isDynamicCall c = liftIO $ FFI.cursor_isDynamicCall c

getCXXAccessSpecifier :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.CXXAccessSpecifier
getCXXAccessSpecifier c = liftIO $ FFI.getCXXAccessSpecifier c

getOverloadedDecls :: ClangBase m => FFI.Cursor s' -> ClangT s m [FFI.Cursor s]
getOverloadedDecls c = liftIO $ do
                         numDecls <- FFI.getNumOverloadedDecls c
                         mapM (FFI.getOverloadedDecl mkProxy c) [0..(numDecls-1)]

-- CursorSet functions

createSet :: ClangBase m => ClangT s m (FFI.CursorSet s)
createSet = FFI.createCXCursorSet

setContains :: ClangBase m => FFI.CursorSet s' -> FFI.Cursor s'' -> ClangT s m Bool
setContains s c = liftIO $ FFI.cXCursorSet_contains s c

setInsert :: ClangBase m => FFI.CursorSet s' -> FFI.Cursor s'' -> ClangT s m Bool
setInsert s c = liftIO $ FFI.cXCursorSet_insert s c


--CursorKind functions

getCursorKindSpelling :: ClangBase m => FFI.CursorKind -> ClangT s m (ClangString s)
getCursorKindSpelling = FFI.getCursorKindSpelling
