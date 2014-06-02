{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
  The Cursor is an easy way to reference something in code and query its properties and relationships
  This is the primary way of traversing and querying code
-}
module Clang.Cursor
( isNullCursor
, nullCursor
, getHash
, getKind
, getLinkage
, getAvailability
, getLanguage
, getTranslationUnit
, getSemanticParent
, getLexicalParent
, getOverriddenCursors
, getIncludedFile
, getLocation
, getSpellingLocation
, getExtent
, getType
, getResultType
, getDeclObjCTypeEncoding
, getSpelling
, getSpellingNameRange
, getDisplayName
, getReferenced
, getDefinition
, getCanonicalCursor
, getObjCSelectorIndex
, getReceiverType
, getObjCPropertyAttributes
, getObjCDeclQualifiers
, isObjCOptional
, isVariadic
, getTemplateKind
, getTemplateForSpecialization
, getReferenceNameRange
, getTypeDeclaration
, getNumArguments
, getArgument
, getUSR

-- attribute function
, getIBOutletCollectionType

, isDefinition
, isDeclaration
, isReference
, isExpression
, isStatement
, isAttribute
, isInvalid
, isTranslationUnit
, isPreprocessing
, isUnexposed
, isBitField
, isVirtualBase
, isPureVirtualCppMethod
, isStaticCppMethod
, isVirtualCppMethod
, isDynamicCall
, getCommentRange
, getRawCommentText
, getBriefCommentText
, getParsedComment
, getModule
, getCXXAccessSpecifier
, getOverloadedDecls

-- CursorSet functions
, createSet
, setContains
, setInsert

-- CursorKind functions
, getCursorKindSpelling

-- Platform availability
, getCursorPlatformAvailability

, getCompletionString
) where

import Control.Applicative
import Control.Monad.IO.Class
import GHC.Word

import Clang.Internal.BitFlags
import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

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

getTranslationUnit :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.TranslationUnit s)
getTranslationUnit = FFI.cursor_getTranslationUnit

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

getDeclObjCTypeEncoding :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ClangString s)
getDeclObjCTypeEncoding = FFI.getDeclObjCTypeEncoding

getSpelling :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ClangString s)
getSpelling = FFI.getCursorSpelling

getSpellingNameRange :: ClangBase m => FFI.Cursor s' -> Int -> ClangT s m (FFI.SourceRange s)
getSpellingNameRange c idx = liftIO $ FFI.cursor_getSpellingNameRange mkProxy c idx

getDisplayName :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ClangString s)
getDisplayName = FFI.getCursorDisplayName

getReferenced :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getReferenced c = liftIO $ FFI.getCursorReferenced mkProxy c

getDefinition :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getDefinition c = liftIO $ FFI.getCursorDefinition mkProxy c

getCanonicalCursor :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getCanonicalCursor c = liftIO $ FFI.getCanonicalCursor mkProxy c

getObjCSelectorIndex :: ClangBase m => FFI.Cursor s' -> ClangT s m Int
getObjCSelectorIndex c = liftIO $ FFI.cursor_getObjCSelectorIndex c

getReceiverType :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Type s)
getReceiverType c = liftIO $ FFI.cursor_getReceiverType mkProxy c

getObjCPropertyAttributes :: ClangBase m => FFI.Cursor s'
                          -> ClangT s m [FFI.ObjCPropertyAttrKind]
getObjCPropertyAttributes c = unFlags <$> liftIO (FFI.cursor_getObjCPropertyAttributes c)

getObjCDeclQualifiers :: ClangBase m => FFI.Cursor s' -> ClangT s m [FFI.ObjCDeclQualifierKind]
getObjCDeclQualifiers c = unFlags <$> liftIO (FFI.cursor_getObjCDeclQualifiers c)

isObjCOptional :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isObjCOptional c = liftIO $ FFI.cursor_isObjCOptional c

isVariadic :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isVariadic c = liftIO $ FFI.cursor_isVariadic c

getTemplateKind :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.CursorKind
getTemplateKind c = liftIO $ FFI.getTemplateCursorKind c

getTemplateForSpecialization :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Cursor s)
getTemplateForSpecialization c = liftIO $ FFI.getSpecializedCursorTemplate mkProxy c

getReferenceNameRange :: ClangBase m => FFI.Cursor s' -> [FFI.NameRefFlags] -> Int
                      -> ClangT s m (FFI.SourceRange s)
getReferenceNameRange c fs p = liftIO $ FFI.getCursorReferenceNameRange mkProxy c (orFlags fs) p

getTypeDeclaration :: ClangBase m => FFI.Type s' -> ClangT s m (FFI.Cursor s)
getTypeDeclaration t = liftIO $ FFI.getTypeDeclaration mkProxy t

getNumArguments :: ClangBase m => FFI.Cursor s' -> ClangT s m Int
getNumArguments c = liftIO $ FFI.cursor_getNumArguments c

getArgument :: ClangBase m => FFI.Cursor s' -> Int -> ClangT s m (FFI.Cursor s)
getArgument c i = liftIO $ FFI.cursor_getArgument mkProxy c i

getUSR :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ClangString s)
getUSR = FFI.getCursorUSR

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

isAttribute :: FFI.CursorKind -> Bool
isAttribute = FFI.isAttribute
{-# INLINE isAttribute #-}

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

isBitField :: ClangBase m => FFI.Cursor s' -> ClangT s m Bool
isBitField c = liftIO $ FFI.isBitField c

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

getCommentRange :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.SourceRange s)
getCommentRange c = liftIO $ FFI.cursor_getCommentRange mkProxy c

getRawCommentText :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ClangString s)
getRawCommentText = FFI.cursor_getRawCommentText

getBriefCommentText :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.ClangString s)
getBriefCommentText = FFI.cursor_getBriefCommentText

getParsedComment :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Comment s)
getParsedComment c = liftIO $ FFI.cursor_getParsedComment mkProxy c

getModule :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.Module s)
getModule c = liftIO $ FFI.cursor_getModule mkProxy c

getCXXAccessSpecifier :: ClangBase m => FFI.Cursor s' -> ClangT s m FFI.CXXAccessSpecifier
getCXXAccessSpecifier c = liftIO $ FFI.getCXXAccessSpecifier c

getOverloadedDecls :: ClangBase m => FFI.Cursor s' -> ClangT s m [FFI.Cursor s]
getOverloadedDecls c = liftIO $ do
  numDecls <- FFI.getNumOverloadedDecls c
  mapM (FFI.getOverloadedDecl mkProxy c) [0..(numDecls - 1)]

-- CursorSet functions

createSet :: ClangBase m => ClangT s m (FFI.CursorSet s)
createSet = FFI.createCXCursorSet

setContains :: ClangBase m => FFI.CursorSet s' -> FFI.Cursor s'' -> ClangT s m Bool
setContains s c = liftIO $ FFI.cXCursorSet_contains s c

setInsert :: ClangBase m => FFI.CursorSet s' -> FFI.Cursor s'' -> ClangT s m Bool
setInsert s c = liftIO $ FFI.cXCursorSet_insert s c


-- CursorKind functions

getCursorKindSpelling :: ClangBase m => FFI.CursorKind -> ClangT s m (FFI.ClangString s)
getCursorKindSpelling = FFI.getCursorKindSpelling

-- Platform availability

getCursorPlatformAvailability :: ClangBase m => FFI.Cursor s'
                              -> ClangT s m (FFI.PlatformAvailabilityInfo s)
getCursorPlatformAvailability = FFI.getCursorPlatformAvailability

-- | Retrieve a completion string for an arbitrary declaration or macro
-- definition cursor.
--
-- Completion strings can be manipulated using the functions in "Clang.Completion".
getCompletionString :: ClangBase m => FFI.Cursor s' -> ClangT s m (FFI.CompletionString s)
getCompletionString = FFI.getCursorCompletionString
