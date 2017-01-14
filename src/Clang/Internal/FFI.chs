{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

module Clang.Internal.FFI
( versionMajor
, versionMinor
, encodedVersion
, Index
, createIndex
, GlobalIndexOptions(..)
, threadBackgroundPriorityForAll
, cXIndex_setGlobalOptions
, cXIndex_getGlobalOptions
, TranslationUnit
, UnsavedFile
, unsavedFilename
, unsavedContents
, newUnsavedFile
, updateUnsavedContents
, AvailabilityKind(..)
, Version(..)
, ClangString
, getString
, getByteString
, unsafeGetByteString
, File(..)
, getFileName
, getFileTime
, UniqueId(..)
, getFileUniqueID
, isFileMultipleIncludeGuarded
, getFile
, SourceLocation
, getNullLocation
, equalLocations
, getLocation
, getLocationForOffset
, location_isInSystemHeader
, location_isFromMainFile
, SourceRange
, getNullRange
, getRange
, equalRanges
, range_isNull
, getExpansionLocation
, getPresumedLocation
, getSpellingLocation
, getFileLocation
, getRangeStart
, getRangeEnd
, Severity(..)
, Diagnostic
, DiagnosticSet
, getNumDiagnosticsInSet
, getDiagnosticInSet
, LoadError(..)
, loadDiagnostics
, getChildDiagnostics
, getNumDiagnostics
, getDiagnostic
, getDiagnosticSetFromTU
, DisplayOptions(..)
, formatDiagnostic
, defaultDiagnosticDisplayOptions
, getDiagnosticSeverity
, getDiagnosticLocation
, getDiagnosticSpelling
, getDiagnosticOption
, getDiagnosticCategory
, getDiagnosticCategoryText
, getDiagnosticNumRanges
, getDiagnosticRange
, getDiagnosticNumFixIts
, getDiagnosticFixIt
, getTranslationUnitSpelling
, createTranslationUnitFromSourceFile
, createTranslationUnit
, TranslationUnitFlags(..)
, defaultEditingTranslationUnitOptions
, parseTranslationUnit
, SaveTranslationUnitFlags(..)
, defaultSaveOptions
, saveTranslationUnit
, ReparseFlags(..)
, defaultReparseOptions
, reparseTranslationUnit
, CursorKind(..)
, firstDeclCursor
, lastDeclCursor
, firstRefCursor
, lastRefCursor
, firstInvalidCursor
, lastInvalidCursor
, firstExprCursor
, lastExprCursor
, firstStmtCursor
, lastStmtCursor
, firstAttrCursor
, lastAttrCursor
, firstPreprocessingCursor
, lastPreprocessingCursor
, firstExtraDeclCursor
, lastExtraDeclCursor
, gccAsmStmtCursor
, macroInstantiationCursor
, Comment(..)
, Cursor
, getNullCursor
, getTranslationUnitCursor
, cursor_isNull
, hashCursor
, getCursorKind
, isDeclaration
, isReference
, isExpression
, isStatement
, isAttribute
, isInvalid
, isTranslationUnit
, isPreprocessing
, isUnexposed
, LinkageKind(..)
, getCursorLinkage
, getCursorAvailability
, PlatformAvailability(..)
, PlatformAvailabilityInfo(..)
, getCursorPlatformAvailability
, LanguageKind(..)
, getCursorLanguage
, cursor_getTranslationUnit
, CursorSet
, createCXCursorSet
, cXCursorSet_contains
, cXCursorSet_insert
, getCursorSemanticParent
, getCursorLexicalParent
, getOverriddenCursors
, getIncludedFile
, getCursor
, getCursorLocation
, getCursorSpellingLocation
, getCursorExtent
, TypeKind(..)
, type_FirstBuiltin
, type_LastBuiltin
, CallingConv(..)
, Type
, getTypeKind
, getCursorType
, getTypeSpelling
, getTypedefDeclUnderlyingType
, getEnumDeclIntegerType
, getEnumConstantDeclValue
, getEnumConstantDeclUnsignedValue
, getFieldDeclBitWidth
, cursor_getNumArguments
, cursor_getArgument
, equalTypes
, getCanonicalType
, isConstQualifiedType
, isVolatileQualifiedType
, isRestrictQualifiedType
, getPointeeType
, getTypeDeclaration
, getDeclObjCTypeEncoding
, getTypeKindSpelling
, getFunctionTypeCallingConv
, getResultType
, getNumArgTypes
, getArgType
, isFunctionTypeVariadic
, getCursorResultType
, isPODType
, getElementType
, getNumElements
, getArrayElementType
, getArraySize
, TypeLayoutError(..)
, type_getAlignOf
, type_getClassType
, type_getSizeOf
, type_getOffsetOf
, RefQualifierKind(..)
, type_getCXXRefQualifier
, isBitField
, isVirtualBase
, CXXAccessSpecifier(..)
, getCXXAccessSpecifier
, getNumOverloadedDecls
, getOverloadedDecl
, getIBOutletCollectionType
, CursorList
, getChildren
, getDescendants
, getDeclarations
, getReferences
, getDeclarationsAndReferences
, ParentedCursor(..)
, ParentedCursorList
, getParentedDescendants
, getParentedDeclarations
, getParentedReferences
, getParentedDeclarationsAndReferences
, getCursorUSR
, constructUSR_ObjCClass
, constructUSR_ObjCCategory
, constructUSR_ObjCProtocol
, constructUSR_ObjCIvar
, constructUSR_ObjCMethod
, constructUSR_ObjCProperty
, getCursorSpelling
, cursor_getSpellingNameRange
, getCursorDisplayName
, getCursorReferenced
, getCursorDefinition
, isCursorDefinition
, cursor_isDynamicCall
, getCanonicalCursor
, cursor_getObjCSelectorIndex
, cursor_getReceiverType
, ObjCPropertyAttrKind(..)
, cursor_getObjCPropertyAttributes
, ObjCDeclQualifierKind(..)
, cursor_getObjCDeclQualifiers
, cursor_isObjCOptional
, cursor_isVariadic
, cursor_getCommentRange
, cursor_getRawCommentText
, cursor_getBriefCommentText
, cursor_getParsedComment
, Module(..)
, cursor_getModule
, module_getASTFile
, module_getParent
, module_getName
, module_getFullName
, module_getNumTopLevelHeaders
, module_getTopLevelHeader
, cXXMethod_isPureVirtual
, cXXMethod_isStatic
, cXXMethod_isVirtual
, getTemplateCursorKind
, getSpecializedCursorTemplate
, getCursorReferenceNameRange
, NameRefFlags(..)
, CommentKind(..)
, InlineCommandRenderStyle(..)
, ParamPassDirectionKind(..)
, comment_getKind
, comment_getNumChildren
, comment_getChild
, comment_isWhitespace
, inlineContentComment_hasTrailingNewline
, textComment_getText
, inlineCommandComment_getCommandName
, inlineCommandComment_getRenderKind
, inlineCommandComment_getNumArgs
, inlineCommandComment_getArgText
, hTMLTagComment_getTagName
, hTMLStartTagComment_isSelfClosing
, hTMLStartTag_getNumAttrs
, hTMLStartTag_getAttrName
, hTMLStartTag_getAttrValue
, blockCommandComment_getCommandName
, blockCommandComment_getNumArgs
, blockCommandComment_getArgText
, blockCommandComment_getParagraph
, paramCommandComment_getParamName
, paramCommandComment_isParamIndexValid
, paramCommandComment_getParamIndex
, paramCommandComment_isDirectionExplicit
, paramCommandComment_getDirection
, tParamCommandComment_getParamName
, tParamCommandComment_isParamPositionValid
, tParamCommandComment_getDepth
, tParamCommandComment_getIndex
, verbatimBlockLineComment_getText
, verbatimLineComment_getText
, hTMLTagComment_getAsString
, fullComment_getAsHTML
, fullComment_getAsXML
, TokenKind(..)
, Token
, TokenList
, getTokenKind
, getTokenSpelling
, getTokenLocation
, getTokenExtent
, tokenize
, annotateTokens
, getCursorKindSpelling
, enableStackTraces
, CompletionString
, CompletionResult
, ChunkKind(..)
, getCompletionChunkKind
, getCompletionChunkText
, getCompletionChunkCompletionString
, getNumCompletionChunks
, getCompletionPriority
, getCompletionAvailability
, getCompletionNumAnnotations
, getCompletionAnnotation
, getCompletionParent
, getCompletionBriefComment
, getCursorCompletionString
, CodeCompleteFlags(..)
, defaultCodeCompleteOptions
, CodeCompleteResults
, codeCompleteAt
, codeCompleteGetNumResults
, codeCompleteGetResult
, sortCodeCompletionResults
, codeCompleteGetNumDiagnostics
, codeCompleteGetDiagnostic
, CompletionContext(..)
, codeCompleteGetContexts
, codeCompleteGetContainerKind
, codeCompleteGetContainerUSR
, codeCompleteGetObjCSelector
, getClangVersion
, toggleCrashRecovery
, Inclusion(..)
, InclusionList
, getInclusions
, Remapping
, getRemappings
, getRemappingsFromFileList
, remap_getNumFiles
, remap_getFilenames
) where

import Control.Monad (forM_)
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Hashable
import Data.Typeable (Typeable)
import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import Data.Word
import Foreign.C
import Foreign
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Unsafe.Coerce (unsafeCoerce)  -- With GHC 7.8 we can use the safer 'coerce'.
import System.IO.Unsafe(unsafePerformIO)

import Clang.Internal.BitFlags
import Clang.Internal.FFIConstants
import Clang.Internal.Monad

#include <inttypes.h>
#include <stdlib.h>
#include <stddef.h>
#include <clang-c/Index.h>
#include <stdio.h>
#include "utils.h"
#include "visitors.h"
#include "wrappers.h"

{-
LibClang uses two strategies to create safe, deterministic
bindings.

First, all types that either represent resources managed on the C side
(for example, TranslationUnit and ClangString) or contain pointers into
those resources (for example, Cursor) are tagged with an ST-like
universally quantified phantom type parameter. This prevents them from
being used outside of the scope in which they were allocated. This is
particularly important for libclang, which uses an arena allocator to
store much of its data; preventing resources from leaking outside
their proper scope is critical to making this safe.

Second, all operations that allocate a resource that later
needs to be freed are wrapped in a type-specific 'register'
function. An example is registerClangString. This function registers a
cleanup action with the underlying ClangT monad, which is essentially
just ResourceT in disguise. This not only provides safety, but it also
enforces prompt finalization of resources which can significantly
increase performance by keeping the working set of user programs down.

There are a few different patterns for the FFI code, depending on what
kind of value the libclang function we want to wrap returns.

If it returns a pure value (say an Int), then we don't need to do
anything special. The greencard-generated function will have a return
type of IO Int, and the user-facing wrapper function will use liftIO
to call it.

If it returns a value that doesn't represent a newly allocated
resource, but does need the phantom type parameter (because it
contains a pointer into some other resource, generally), then we
generally pass a Proxy value with a type parameter that we'll use to
tag the return type. This has no runtime cost. The user-facing wrapper
function still needs to call liftIO.

For values that DO represent a newly allocated resource, we need to
call the appropriate 'register' function. This function will return a
value in the ClangT monad, so user-facing wrapper functions don't need
to use liftIO or Proxy in this case. It's the convention to use '()' for the
phantom type parameter returned from the greencard-generated function,
and allow the 'register' function to coerce the value to the correct
type. This way we can distinguish values that need to be registered
from other values: if a value's phantom type parameter is '()', it
needs to be registered, and it won't typecheck as the return value of
a user-facing wrapper function.

It's important to keep in mind that it should be legal to use values
from enclosing scopes in an inner scope created by clangScope. In
practice, this means that the phantom type parameters used by each
argument to a function should be distinct, and they should all be
distinct from the phantom type parameter used in the return value.
Of course, this doesn't apply to the Proxy parameter, which must
always have the same phantom type parameter as the return value.
-}

-- Marshalling utility functions.
fromCInt :: Num b => CInt -> b
fromCInt = fromIntegral

toCInt :: Integral a => a -> CInt
toCInt = fromIntegral

-- Version information.
versionMajor :: Int
versionMajor   = {# const CINDEX_VERSION_MAJOR #}
versionMinor :: Int
versionMinor   = {# const CINDEX_VERSION_MINOR #}
encodedVersion :: Int
encodedVersion = {# const CINDEX_VERSION #}

-- typedef void *CXIndex;
newtype Index s = Index { unIndex :: Ptr () }
                  deriving (Eq, Ord, Typeable)

instance ClangValue Index

mkIndex :: Ptr () -> Index ()
mkIndex = Index

-- CXIndex clang_createIndex(int excludeDeclarationsFromPCH, int displayDiagnostics);
{# fun clang_createIndex { `CInt', `CInt' } -> `Ptr ()' #}
unsafe_createIndex :: Bool -> Bool -> IO (Index ())
unsafe_createIndex a b = clang_createIndex (fromBool a) (fromBool b) >>= return . mkIndex

createIndex :: ClangBase m => Bool -> Bool -> ClangT s m (Index s)
createIndex = (registerIndex .) . unsafe_createIndex


-- void clang_disposeIndex(CXIndex index);
{# fun clang_disposeIndex { `Ptr ()' } -> `()' #}
disposeIndex :: Index s -> IO ()
disposeIndex index = clang_disposeIndex (unIndex index)

registerIndex :: ClangBase m => IO (Index ()) -> ClangT s m (Index s)
registerIndex action = do
  (_, idx) <- clangAllocate (action >>= return . unsafeCoerce)
                            (\i -> disposeIndex i)
  return idx
{-# INLINEABLE registerIndex #-}

-- typedef enum {
--   CXGlobalOpt_None = 0x0,
--   CXGlobalOpt_ThreadBackgroundPriorityForIndexing = 0x1,
--   CXGlobalOpt_ThreadBackgroundPriorityForEditing = 0x2,
--   CXGlobalOpt_ThreadBackgroundPriorityForAll =
--       CXGlobalOpt_ThreadBackgroundPriorityForIndexing |
--       CXGlobalOpt_ThreadBackgroundPriorityForEditing
--
-- } CXGlobalOptFlags;

-- | Options that apply globally to every translation unit within an index.
#c
enum GlobalIndexOptions
   { DefaultGlobalIndexOptions = CXGlobalOpt_None
   , ThreadBackgroundPriorityForIndexing = CXGlobalOpt_ThreadBackgroundPriorityForIndexing
   , ThreadBackgroundPriorityForEditing = CXGlobalOpt_ThreadBackgroundPriorityForEditing
   };
#endc
{# enum GlobalIndexOptions{} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags GlobalIndexOptions where
  toBit DefaultGlobalIndexOptions           = 0x0
  toBit ThreadBackgroundPriorityForIndexing = 0x1
  toBit ThreadBackgroundPriorityForEditing  = 0x2

-- | A set of global index options that requests background priority for all threads.
threadBackgroundPriorityForAll :: [GlobalIndexOptions]
threadBackgroundPriorityForAll = [ThreadBackgroundPriorityForEditing,
                                  ThreadBackgroundPriorityForIndexing]

-- void clang_CXIndex_setGlobalOptions(CXIndex, unsigned options);
{# fun clang_CXIndex_setGlobalOptions { `Ptr ()', `Int' } -> `()' #}
cXIndex_setGlobalOptions :: Index s -> Int -> IO ()
cXIndex_setGlobalOptions index options = clang_CXIndex_setGlobalOptions (unIndex index) options

-- unsigned clang_CXIndex_getGlobalOptions(CXIndex);
{# fun clang_CXIndex_getGlobalOptions { `Ptr ()' } -> `Int' #}
cXIndex_getGlobalOptions :: Index s -> IO Int
cXIndex_getGlobalOptions index = clang_CXIndex_getGlobalOptions (unIndex index)

-- typedef struct CXTranslationUnitImpl *CXTranslationUnit;
newtype TranslationUnit s = TranslationUnit { unTranslationUnit :: (Ptr ()) }
                            deriving (Eq, Ord, Typeable)

instance ClangValue TranslationUnit

mkTranslationUnit :: Ptr () -> TranslationUnit ()
mkTranslationUnit = TranslationUnit

-- void clang_disposeTranslationUnit(CXTranslationUnit);
{# fun clang_disposeTranslationUnit { `Ptr ()' } -> `()' #}
disposeTranslationUnit :: TranslationUnit s -> IO ()
disposeTranslationUnit t = clang_disposeTranslationUnit (unTranslationUnit t)

registerTranslationUnit :: ClangBase m => IO (TranslationUnit ())
                        -> ClangT s m (TranslationUnit s)
registerTranslationUnit action = do
  (_, tu) <- clangAllocate (action >>= return . unsafeCoerce)
                           (\t -> disposeTranslationUnit t)
  return tu
{-# INLINEABLE registerTranslationUnit #-}

-- struct CXUnsavedFile {
--   const char* Filename;
--   const char* Contents;
--   unsigned long Length;
-- };

-- | A representation of an unsaved file and its contents.
data UnsavedFile = UnsavedFile
  { _unsavedFilename :: !B.ByteString
  , _unsavedContents :: !B.ByteString
  } deriving (Eq, Show, Typeable)

-- We maintain the invariant that _unsavedFilename is always
-- null-terminated. That's why we don't directly expose the record fields.
unsavedFilename :: UnsavedFile -> B.ByteString
unsavedFilename = _unsavedFilename

unsavedContents :: UnsavedFile -> B.ByteString
unsavedContents = _unsavedContents

newUnsavedFile :: B.ByteString -> B.ByteString -> UnsavedFile
newUnsavedFile f c = UnsavedFile (nullTerminate f) c

updateUnsavedContents :: UnsavedFile -> B.ByteString -> UnsavedFile
updateUnsavedContents uf c = UnsavedFile (unsavedFilename uf) c

-- TODO: Use BU.unsafeLast when we can use newer B.ByteString.
nullTerminate :: B.ByteString -> B.ByteString
nullTerminate bs
  | B.null bs      = B.singleton 0
  | B.last bs == 0 = bs
  | otherwise      = B.snoc bs 0

-- Functions which take a Vector UnsavedFile argument are implemented
-- internally in terms of this function, which temporarily allocates a
-- Vector CUnsavedFile holding the same data. (But does not copy the
-- string data itself.)
withUnsavedFiles :: DV.Vector UnsavedFile -> (Ptr CUnsavedFile -> Int -> IO a) -> IO a
withUnsavedFiles ufs f =
  withCUnsavedFiles ufs $ \cufs ->
    DVS.unsafeWith cufs $ \ptr ->
      f ptr (DVS.length cufs)

data CUnsavedFile = CUnsavedFile
  { cUnsavedFilename    :: CString
  , cUnsavedContents    :: CString
  , cUnsavedContentsLen :: CULong
  }

instance Storable CUnsavedFile where
    sizeOf _ = sizeOfCXUnsavedFile
    {-# INLINE sizeOf #-}

    alignment _ = alignOfCXUnsavedFile
    {-# INLINE alignment #-}

    peek p = do
      filename    <- peekByteOff p offsetCXUnsavedFileFilename
      contents    <- peekByteOff p offsetCXUnsavedFileContents
      contentsLen <- peekByteOff p offsetCXUnsavedFileContentsLen
      return $! CUnsavedFile filename contents contentsLen
    {-# INLINE peek #-}

    poke p (CUnsavedFile filename contents contentsLen) = do
      pokeByteOff p offsetCXUnsavedFileFilename filename
      pokeByteOff p offsetCXUnsavedFileContents contents
      pokeByteOff p offsetCXUnsavedFileContentsLen contentsLen
    {-# INLINE poke #-}


withCUnsavedFiles :: DV.Vector UnsavedFile -> (DVS.Vector CUnsavedFile -> IO a) -> IO a
withCUnsavedFiles ufs f = do
    let len = DV.length ufs
    v <- DVSM.new len
    go v 0 len
  where
    go v i len
      | i == len  = f =<< DVS.unsafeFreeze v
      | otherwise = do let uf = DV.unsafeIndex ufs i
                           ufFilename = unsavedFilename uf
                           ufContents = unsavedContents uf
                       BU.unsafeUseAsCString ufFilename $ \cufFilename ->
                         BU.unsafeUseAsCString ufContents $ \cufContents -> do
                           let contentsLen = fromIntegral $ B.length ufContents
                               cuf = CUnsavedFile cufFilename cufContents contentsLen
                           DVSM.write v i cuf
                           go v (i + 1) len

#c
enum AvailabilityKind {
  Availability_Available = CXAvailability_Available,
  Availability_Deprecated = CXAvailability_Deprecated,
  Availability_NotAvailable = CXAvailability_NotAvailable,
  Availability_NotAccessible = CXAvailability_NotAccessible
};
#endc
{# enum AvailabilityKind{} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

data Version = Version
  { majorVersion    :: !Int
  , minorVersion    :: !Int
  , subminorVersion :: !Int
  } deriving (Eq, Ord, Show, Typeable)

instance Storable Version where
  sizeOf _ = sizeOfCXVersion
  {-# INLINE sizeOf #-}

  alignment _ = alignOfCXVersion
  {-# INLINE alignment #-}

  peek p = do
    major <- fromCInt <$> peekByteOff p offsetCXVersionMajor
    minor <- fromCInt <$> peekByteOff p offsetCXVersionMinor
    subminor <- fromCInt <$> peekByteOff p offsetCXVersionSubminor
    return $! Version major minor subminor
  {-# INLINE peek #-}

  poke p (Version major minor subminor) = do
    pokeByteOff p offsetCXVersionMajor major
    pokeByteOff p offsetCXVersionMinor minor
    pokeByteOff p offsetCXVersionSubminor subminor
  {-# INLINE poke #-}

-- typedef struct {
--   const void *data;
--   unsigned private_flags;
-- } CXString;
data ClangString s = ClangString !(Ptr ()) !Word32
                     deriving (Eq, Ord, Typeable)

instance ClangValue ClangString

instance Storable (ClangString s) where
  sizeOf _ = {# sizeof CXString #} -- sizeOfCXString
  {-# INLINE sizeOf #-}

  alignment _ = {# alignof CXString #} -- alignOfCXString
  {-# INLINE alignment #-}

  peek p = do
    strData <- {#get CXString->data #} p
    strFlags <- {#get CXString->private_flags #} p
    return $! ClangString strData (fromIntegral strFlags)
  {-# INLINE peek #-}

  poke p (ClangString d f) = do
    {#set CXString->data #} p d
    {#set CXString->private_flags #} p (fromIntegral f)
  {-# INLINE poke #-}

instance Hashable (ClangString s) where
  hashWithSalt salt (ClangString p f) = (`hashWithSalt` f)
                                      . (`hashWithSalt` pInt)
                                      $ salt
    where
      pInt = (fromIntegral $ ptrToWordPtr p) :: Int

registerClangString :: ClangBase m => IO (ClangString ()) -> ClangT s m (ClangString s)
registerClangString action = do
  (_, str) <- clangAllocate (action >>= return . unsafeCoerce)
                            (\(ClangString d f) -> freeClangString d f)
  return str
{-# INLINEABLE registerClangString #-}

{#fun freeClangString { id `Ptr ()', `Word32' } -> `()' #}

unmarshall_clangString :: Ptr () -> Word32 -> IO (ClangString ())
unmarshall_clangString d f = return $ ClangString d f

getString :: ClangBase m => ClangString s' -> ClangT s m String
getString (ClangString d f) = liftIO $ getCStringPtr d f >>= peekCString

getByteString :: ClangBase m => ClangString s' -> ClangT s m B.ByteString
getByteString (ClangString d f) = liftIO $ getCStringPtr d f >>= B.packCString

unsafeGetByteString :: ClangBase m => ClangString s' -> ClangT s m B.ByteString
unsafeGetByteString (ClangString d f) = liftIO $ getCStringPtr d f >>= BU.unsafePackCString

-- const char *clang_getCString(ClangString string);
{# fun clang_getCString {withVoided* %`ClangString a' } -> `CString' id #}
getCStringPtr :: Ptr () -> Word32 -> IO CString
getCStringPtr d f = clang_getCString (ClangString d f)

-- typedef void *CXFile;
newtype File s = File { unFile :: Ptr () }
                 deriving (Eq, Ord, Typeable)

instance ClangValue File

instance Hashable (File s) where
  hashWithSalt salt (File p) = let !pInt = (fromIntegral $ ptrToWordPtr p) :: Int
                               in  hashWithSalt salt pInt

maybeFile :: File s' -> Maybe (File s)
maybeFile (File p) | p == nullPtr = Nothing
maybeFile f                       = Just (unsafeCoerce f)

unMaybeFile :: Maybe (File s') -> File s
unMaybeFile (Just f) = unsafeCoerce f
unMaybeFile Nothing  = File nullPtr


-- CXString clang_getFileName(CXFile SFile);
{# fun wrapped_clang_getFileName as clang_getFileName { `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getFileName :: File s -> IO (ClangString ())
unsafe_getFileName x = clang_getFileName (unFile x) >>= peek

getFileName :: ClangBase m => File s' -> ClangT s m (ClangString s)
getFileName = registerClangString . unsafe_getFileName

-- time_t clang_getFileTime(CXFile SFile);
foreign import ccall unsafe "clang-c/Index.h clang_getFileTime" clang_getFileTime :: Ptr () -> IO CTime

getFileTime :: File s -> IO CTime
getFileTime (File ptr) = clang_getFileTime ptr

-- | A unique identifier that can be used to distinguish 'File's.
data UniqueId = UniqueId !Word64 !Word64 !Word64
                deriving (Eq, Ord, Show, Typeable)

instance Hashable UniqueId where
    hashWithSalt salt (UniqueId a b c) = (`hashWithSalt` a)
                                       . (`hashWithSalt` b)
                                       . (`hashWithSalt` c)
                                       $ salt
    {-# INLINE hashWithSalt #-}

maybeFileUniqueID :: (Int, Word64, Word64, Word64) -> Maybe UniqueId
maybeFileUniqueID (v, d1, d2, d3) | v /= 0    = Nothing
                                  | otherwise = Just $ UniqueId d1 d2 d3

-- int clang_getFileUniqueID(CXFile file, CXFileUniqueID *outID);
{# fun clang_getFileUniqueID { `Ptr ()', `Ptr ()' } -> `CInt' id #}
getFileUniqueID :: File s -> IO (Maybe UniqueId)
getFileUniqueID f =
  allocaArray 3 (ptrToFileUniqueId f)
  where
    ptrToFileUniqueId :: File s -> Ptr Word64 -> IO (Maybe UniqueId)
    ptrToFileUniqueId f' ptr = do
       res' <- clang_getFileUniqueID (unFile f') (castPtr ptr)
       ds' <- peekArray 3 ptr
       return (maybeFileUniqueID (fromIntegral res', ds' !! 0, ds' !! 1, ds' !! 2))

-- -- unsigned clang_isFileMultipleIncludeGuarded(CXTranslationUnit tu, CXFile file);
{# fun clang_isFileMultipleIncludeGuarded { `Ptr ()', `Ptr ()' } -> `CInt' #}
isFileMultipleIncludeGuarded :: TranslationUnit s -> File s' -> IO Bool
isFileMultipleIncludeGuarded t f = clang_isFileMultipleIncludeGuarded (unTranslationUnit t) (unFile f) >>= return . (toBool :: Int -> Bool) . fromIntegral

-- CXFile clang_getFile(CXTranslationUnit tu, const char *file_name);
{# fun clang_getFile { `Ptr ()', `CString' } -> `Ptr ()' #}
getFile:: Proxy s -> TranslationUnit s' -> String -> IO (File s)
getFile _ t s = withCString s (\cs -> clang_getFile (unTranslationUnit t) cs >>= return . File)

-- typedef struct {
--   void *ptr_data[2];
--   unsigned int_data;
-- } CXSourceLocation;

data SourceLocation s = SourceLocation !(Ptr ()) !(Ptr ()) !Int
                        deriving (Ord, Typeable)

instance ClangValue SourceLocation

instance Eq (SourceLocation s) where
  a == b = unsafePerformIO $ equalLocations a b

instance Storable (SourceLocation s) where
    sizeOf _ = sizeOfCXSourceLocation
    {-# INLINE sizeOf #-}

    alignment _ = alignOfCXSourceLocation
    {-# INLINE alignment #-}

    peek p = do
      ptrArray <- {#get CXSourceLocation->ptr_data #} p >>= peekArray 2
      intData <- {#get CXSourceLocation->int_data #} p
      return $! SourceLocation (ptrArray !! 0) (ptrArray !! 1) (fromIntegral intData)
    {-# INLINE peek #-}

    poke p (SourceLocation p0 p1 i )= do
      ptrsArray <- mallocArray 2
      pokeArray ptrsArray [p0,p1]
      {#set CXSourceLocation->ptr_data #} p (castPtr ptrsArray)
      {#set CXSourceLocation->int_data #} p (fromIntegral i)
    {-# INLINE poke #-}

-- typedef struct {
--   void *ptr_data[2];
--   unsigned begin_int_data;
--   unsigned end_int_data;
-- } CXSourceRange;
data SourceRange s = SourceRange !(Ptr ()) !(Ptr ()) !Int !Int
                     deriving (Ord, Typeable)

instance ClangValue SourceRange

instance Eq (SourceRange s) where
  a == b = unsafePerformIO $ equalRanges a b

instance Storable (SourceRange s) where
    sizeOf _ = sizeOfCXSourceRange
    {-# INLINE sizeOf #-}

    alignment _ = alignOfCXSourceRange
    {-# INLINE alignment #-}

    peek p = do
      ptrArray <- {#get CXSourceRange->ptr_data #} p >>= peekArray 2
      beginIntData <- {#get CXSourceRange->begin_int_data #} p
      endIntData <- {#get CXSourceRange->end_int_data #} p
      return $! SourceRange (ptrArray !! 0) (ptrArray !! 1) (fromIntegral beginIntData) (fromIntegral endIntData)
    {-# INLINE peek #-}

    poke p (SourceRange p0 p1 begin end)= do
      ptrsArray <- mallocArray 2
      pokeArray ptrsArray [p0,p1]
      {#set CXSourceRange->ptr_data #} p (castPtr ptrsArray)
      {#set CXSourceRange->begin_int_data #} p (fromIntegral begin)
      {#set CXSourceRange->end_int_data #} p (fromIntegral end)
    {-# INLINE poke #-}

-- CXSourceLocation wrapped_clang_getNullLocation();
{# fun wrapped_clang_getNullLocation as clang_getNullLocation { } -> `Ptr (SourceLocation s)' castPtr #}
getNullLocation :: Proxy s -> IO (SourceLocation s)
getNullLocation _ = clang_getNullLocation >>= peek

withVoided :: Storable a => a -> (Ptr () -> IO c) -> IO c
withVoided a f= with a (\aPtr -> f (castPtr aPtr))

-- unsigned clang_equalLocations(CXSourceLocation loc1, CXSourceLocation loc2);
{# fun clang_equalLocations {withVoided* %`SourceLocation a' , withVoided* %`SourceLocation b' } -> `Bool' toBool #}
equalLocations :: SourceLocation s -> SourceLocation s' -> IO Bool
equalLocations s1 s2 = clang_equalLocations s1 s2

-- CXSourceLocation clang_getLocation(CXTranslationUnit tu,
--                                                   CXFile file,
--                                                   unsigned line,
--                                                   unsigned column);
{# fun wrapped_clang_getLocation as clang_getLocation { `Ptr ()' , `Ptr ()', `Int', `Int' } -> `Ptr (SourceLocation s)' castPtr #}
getLocation :: Proxy s -> TranslationUnit s' -> File s'' -> Int -> Int -> IO (SourceLocation s)
getLocation _ t f i j = clang_getLocation (unTranslationUnit t) (unFile f) i j >>= peek

-- CXSourceLocation clang_getLocationForOffset(CXTranslationUnit tu,
--                                                            CXFile file,
--                                                            unsigned offset);
{# fun wrapped_clang_getLocationForOffset as clang_getLocationForOffset { `Ptr ()', `Ptr ()', `Int' } -> `Ptr (SourceLocation s)' castPtr #}
getLocationForOffset :: Proxy s -> TranslationUnit s' -> File s'' -> Int -> IO (SourceLocation s)
getLocationForOffset _ t f i = clang_getLocationForOffset (unTranslationUnit t) (unFile f) i >>= peek

-- int clang_Location_isInSystemHeader(CXSourceLocation location);
{# fun clang_Location_isInSystemHeader { withVoided* %`SourceLocation a' } -> `Bool' toBool #}
location_isInSystemHeader :: SourceLocation s -> IO Bool
location_isInSystemHeader s = clang_Location_isInSystemHeader s

-- int clang_Location_isFromMainFile(CXSourceLocation location);
{# fun clang_Location_isFromMainFile { withVoided* %`SourceLocation a' } -> `Bool' toBool #}
location_isFromMainFile :: SourceLocation s -> IO Bool
location_isFromMainFile s = clang_Location_isFromMainFile s

-- CXSourceRange clang_getNullRange();
{# fun wrapped_clang_getNullRange as clang_getNullRange { } -> `Ptr (SourceRange s)' castPtr #}
getNullRange :: Proxy s -> IO (SourceRange s)
getNullRange _ = clang_getNullRange >>= peek

-- CXSourceRange clang_getRange(CXSourceLocation begin, CXSourceLocation end);
{# fun wrapped_clang_getRange as clang_getRange { withVoided* %`SourceLocation a', withVoided* %`SourceLocation b' } -> `Ptr (SourceRange s)' castPtr #}
getRange :: Proxy s -> SourceLocation s' -> SourceLocation s'' -> IO (SourceRange s)
getRange _ sl1 sl2 = clang_getRange sl1 sl2 >>= peek

-- unsigned clang_equalRanges(CXSourceRange range1, CXSourceRange range2);
{# fun clang_equalRanges {withVoided* %`SourceRange a', withVoided* %`SourceRange b' } -> `Bool' toBool #}
equalRanges :: SourceRange s' -> SourceRange s'' -> IO Bool
equalRanges sr1 sr2 = clang_equalRanges sr1 sr2

-- int clang_Range_isNull(CXSourceRange range);
{# fun clang_Range_isNull {withVoided* %`SourceRange a ' } -> `Bool' toBool #}
range_isNull :: SourceRange s -> IO Bool
range_isNull s = clang_Range_isNull s

#c
typedef CXFile** PtrPtrCXFile;
#endc

-- void clang_getExpansionLocation(CXSourceLocation location,
--                                 CXFile *file,
--                                 unsigned *line,
--                                 unsigned *column,
--                                 unsigned *offset);
{# fun clang_getExpansionLocation { withVoided* %`SourceLocation a', id `Ptr (Ptr ())', id `Ptr CUInt', id `Ptr CUInt', id `Ptr CUInt' } -> `()' #}
getExpansionLocation :: Proxy s -> SourceLocation s' -> IO (Maybe (File s), Int, Int, Int)
getExpansionLocation _ s =
  allocaBytes {#sizeof PtrPtrCXFile #} (\ptrToFilePtr ->
  alloca (\(linePtr :: (Ptr CUInt)) ->
  alloca (\(columnPtr :: (Ptr CUInt)) ->
  alloca (\(offsetPtr :: (Ptr CUInt)) -> do
    clang_getExpansionLocation s (castPtr ptrToFilePtr) (castPtr linePtr) (castPtr columnPtr) (castPtr offsetPtr)
    filePtr <- {#get *CXFile #} ptrToFilePtr
    let _maybeFile = maybeFile (File filePtr)
    line <- peek linePtr
    column <- peek columnPtr
    offset <- peek offsetPtr
    return (_maybeFile, fromIntegral line, fromIntegral column, fromIntegral offset)))))

-- void clang_getPresumedLocation(CXSourceLocation location,
--                                CXString *filename,
--                                unsigned *line,
--                                unsigned *column);

{# fun clang_getPresumedLocation { withVoided* %`SourceLocation a', id `Ptr ()', alloca- `CUInt' peek*, alloca- `CUInt' peek* } -> `()' #}
unsafe_getPresumedLocation :: SourceLocation s' -> IO (ClangString (), Int, Int)
unsafe_getPresumedLocation s =
  alloca (\(stringPtr :: (Ptr (ClangString ()))) -> do
    (line, column) <- clang_getPresumedLocation s (castPtr stringPtr)
    clangString <- peek stringPtr
    return (clangString, fromIntegral line, fromIntegral column))

getPresumedLocation :: ClangBase m => SourceLocation s' -> ClangT s m (ClangString s, Int, Int)
getPresumedLocation l = do
  (f, ln, c) <- liftIO $ unsafe_getPresumedLocation l
  (,,) <$> registerClangString (return f) <*> return ln <*> return c

-- void clang_getSpellingLocation(CXSourceLocation location,
--                                CXFile *file,
--                                unsigned *line,
--                                unsigned *column,
--                                unsigned *offset);
{# fun clang_getSpellingLocation { withVoided* %`SourceLocation a', id `Ptr (Ptr ())', id `Ptr CUInt', id `Ptr CUInt', id `Ptr CUInt' } -> `()' #}
getSpellingLocation :: Proxy s -> SourceLocation s' -> IO (Maybe (File s), Int, Int, Int)
getSpellingLocation _ s =
  allocaBytes {# sizeof PtrPtrCXFile #} (\ptrToFilePtr ->
  alloca (\(linePtr :: (Ptr CUInt)) ->
  alloca (\(columnPtr :: (Ptr CUInt)) ->
  alloca (\(offsetPtr :: (Ptr CUInt)) -> do
    clang_getSpellingLocation s (castPtr ptrToFilePtr) linePtr columnPtr offsetPtr
    filePtr <- {#get *CXFile #} ptrToFilePtr
    let _maybeFile = maybeFile (File filePtr)
    line <- peek linePtr
    column <- peek columnPtr
    offset <- peek offsetPtr
    return (_maybeFile, fromIntegral line, fromIntegral column, fromIntegral offset)))))

-- void clang_getFileLocation(CXSourceLocation location,
--                            CXFile *file,
--                            unsigned *line,
--                            unsigned *column,
--                            unsigned *offset);
{# fun clang_getFileLocation { withVoided* %`SourceLocation a', id `Ptr (Ptr ())', id `Ptr CUInt', id `Ptr CUInt', id `Ptr CUInt' } -> `()' #}
getFileLocation :: Proxy s -> SourceLocation s' -> IO (Maybe (File s), Int, Int, Int)
getFileLocation _ s =
  allocaBytes {#sizeof PtrPtrCXFile #} (\ptrToFilePtr ->
  alloca (\(linePtr :: (Ptr CUInt)) ->
  alloca (\(columnPtr :: (Ptr CUInt)) ->
  alloca (\(offsetPtr :: (Ptr CUInt)) -> do
    clang_getFileLocation s ptrToFilePtr (castPtr linePtr) (castPtr columnPtr) (castPtr offsetPtr)
    filePtr <- {#get *CXFile #} ptrToFilePtr
    let _maybeFile = maybeFile (File filePtr)
    line <- peek linePtr
    column <- peek columnPtr
    offset <- peek offsetPtr
    return (_maybeFile, fromIntegral line, fromIntegral column, fromIntegral offset)))))

-- CXSourceLocation clang_getRangeStart(CXSourceRange range);
{# fun wrapped_clang_getRangeStart as clang_getRangeStart {withVoided* %`SourceRange a' } -> `Ptr (SourceLocation s)' castPtr #}
getRangeStart :: Proxy s -> SourceRange s' -> IO (SourceLocation s)
getRangeStart _ sr = clang_getRangeStart sr >>= peek

-- CXSourceLocation clang_getRangeEnd(CXSourceRange range);
{# fun wrapped_clang_getRangeEnd as clang_getRangeEnd {withVoided* %`SourceRange a' } -> `Ptr (SourceLocation s)' castPtr #}
getRangeEnd :: Proxy s -> SourceRange s' -> IO (SourceLocation s)
getRangeEnd _ sr = clang_getRangeEnd sr >>= peek

-- enum CXDiagnosticSeverity {
--   CXDiagnostic_Ignored = 0,
--   CXDiagnostic_Note    = 1,
--   CXDiagnostic_Warning = 2,
--   CXDiagnostic_Error   = 3,
--   CXDiagnostic_Fatal   = 4
-- };

-- | The severity of a diagnostic.
#c
enum Severity
 { SeverityIgnored = CXDiagnostic_Ignored
 , SeverityNote    = CXDiagnostic_Note
 , SeverityWarning = CXDiagnostic_Warning
 , SeverityError   = CXDiagnostic_Error
 , SeverityFatal   = CXDiagnostic_Fatal
 };
#endc
{# enum Severity {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- typedef void* CXDiagnostic;
newtype Diagnostic s = Diagnostic { unDiagnostic :: Ptr () }
                       deriving (Eq, Ord, Typeable)

instance ClangValue Diagnostic

mkDiagnostic :: Ptr () -> Diagnostic ()
mkDiagnostic = Diagnostic

-- void clang_disposeDiagnostic(CXDiagnostic);
{# fun clang_disposeDiagnostic { id `Ptr ()' } -> `()' #}
disposeDiagnostic:: Diagnostic s -> IO ()
disposeDiagnostic d = clang_disposeDiagnostic (unDiagnostic d)

registerDiagnostic :: ClangBase m => IO (Diagnostic ()) -> ClangT s m (Diagnostic s)
registerDiagnostic action = do
  (_, idx) <- clangAllocate (action >>= return . unsafeCoerce)
                            (\i -> disposeDiagnostic i)
  return idx
{-# INLINEABLE registerDiagnostic #-}

-- typedef void* CXDiagnosticSet;
newtype DiagnosticSet s = DiagnosticSet { unDiagnosticSet :: Ptr () }
                          deriving (Eq, Ord, Typeable)

instance ClangValue DiagnosticSet

mkDiagnosticSet :: Ptr () -> DiagnosticSet ()
mkDiagnosticSet = DiagnosticSet

-- void clang_disposeDiagnosticSet(CXDiagnosticSet);
{# fun clang_disposeDiagnosticSet { id `Ptr ()' } -> `()' #}
disposeDiagnosticSet :: DiagnosticSet s -> IO ()
disposeDiagnosticSet s = clang_disposeDiagnosticSet (unDiagnosticSet s)

registerDiagnosticSet :: ClangBase m => IO (DiagnosticSet ()) -> ClangT s m (DiagnosticSet s)
registerDiagnosticSet action = do
  (_, idx) <- clangAllocate (action >>= return . unsafeCoerce)
                            (\i -> disposeDiagnosticSet i)
  return idx
{-# INLINEABLE registerDiagnosticSet #-}

-- unsigned clang_getNumDiagnosticsInSet(CXDiagnosticSet Diags);
{# fun clang_getNumDiagnosticsInSet { id `Ptr ()' } -> `Int' #}
getNumDiagnosticsInSet :: DiagnosticSet s -> IO Int
getNumDiagnosticsInSet s = clang_getNumDiagnosticsInSet (unDiagnosticSet s)

-- CXDiagnostic clang_getDiagnosticInSet(CXDiagnosticSet Diags, unsigned Index);
{# fun clang_getDiagnosticInSet { id `Ptr ()', `Int' } -> `Ptr ()' id #}
unsafe_getDiagnosticInSet :: DiagnosticSet s -> Int -> IO (Diagnostic ())
unsafe_getDiagnosticInSet s i = clang_getDiagnosticInSet (unDiagnosticSet s) i >>= return . mkDiagnostic

getDiagnosticInSet :: ClangBase m => DiagnosticSet s' -> Int -> ClangT s m (Diagnostic s)
getDiagnosticInSet = (registerDiagnostic .) . unsafe_getDiagnosticInSet

-- enum CXLoadDiag_Error {
--   CXLoadDiag_None = 0,
--   CXLoadDiag_Unknown = 1,
--   CXLoadDiag_CannotLoad = 2,
--   CXLoadDiag_InvalidFile = 3
-- };

-- | An error encountered while loading a serialized diagnostics bitcode file.
#c
enum LoadError
 { LoadSuccessful   = CXLoadDiag_None
 , LoadUnknownError = CXLoadDiag_Unknown
 , LoadCannotOpen   = CXLoadDiag_CannotLoad
 , LoadInvalidFile  = CXLoadDiag_InvalidFile
 };
#endc
{# enum LoadError {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

data LoadDiagsResult =
  LoadDiagsResult LoadError (ClangString ()) (DiagnosticSet ())

-- CXDiagnosticSet clang_loadDiagnostics(const char *file,
--                                       enum CXLoadDiag_Error *error,
--                                       CXString *errorString);
{# fun clang_loadDiagnostics {`CString', alloca- `CInt' peek*, `Ptr ()' } -> `Ptr ()' id #}
unsafe_loadDiagnostics :: FilePath -> IO LoadDiagsResult
unsafe_loadDiagnostics file = withCString file (\cString -> alloca (go cString))
  where
    go :: CString -> Ptr (ClangString ()) -> IO LoadDiagsResult
    go str ptr = do
       (diagnosticSetPtr, err) <- clang_loadDiagnostics str (castPtr ptr)
       errString <- peek (castPtr ptr)
       return (LoadDiagsResult (toEnum (fromIntegral err)) errString (DiagnosticSet diagnosticSetPtr))

loadDiagnostics :: ClangBase m => FilePath
                -> ClangT s m (Either (LoadError, ClangString s) (DiagnosticSet s))
loadDiagnostics path = do
  result <- liftIO $ unsafe_loadDiagnostics path
  case result of
    (LoadDiagsResult err errStr ds@(DiagnosticSet p))
      | p == nullPtr -> Left . (err,) <$> registerClangString (return errStr)
      | otherwise    -> Right <$> registerDiagnosticSet (return ds)

-- CXDiagnosticSet clang_getChildDiagnostics(CXDiagnostic D);
{# fun clang_getChildDiagnostics { `Ptr ()' } -> `Ptr ()' id #}
unsafe_getChildDiagnostics :: Diagnostic s' -> IO (DiagnosticSet ())
unsafe_getChildDiagnostics d = clang_getChildDiagnostics (unDiagnostic d) >>= return . mkDiagnosticSet

-- Note that as a special case, the DiagnosticSet returned by this
-- function does not need to be freed, so we intentionally don't
-- register it.
getChildDiagnostics :: ClangBase m => Diagnostic s' -> ClangT s m (DiagnosticSet s)
getChildDiagnostics = unsafeCoerce <$> unsafe_getChildDiagnostics

-- unsigned clang_getNumDiagnostics(CXTranslationUnit Unit);
{# fun clang_getNumDiagnostics { `Ptr ()' } -> `CUInt' id#}
getNumDiagnostics :: TranslationUnit s -> IO Int
getNumDiagnostics t = clang_getNumDiagnostics (unTranslationUnit t) >>= return . fromIntegral

-- CXDiagnostic clang_getDiagnostic(CXTranslationUnit Unit, unsigned Index);
{# fun clang_getDiagnostic { `Ptr ()', `CUInt' } -> `Ptr ()' id #}
unsafe_getDiagnostic :: TranslationUnit s -> Int -> IO (Diagnostic ())
unsafe_getDiagnostic t i = clang_getDiagnostic (unTranslationUnit t) (fromIntegral i) >>= return . mkDiagnostic

getDiagnostic :: ClangBase m => TranslationUnit s' -> Int -> ClangT s m (Diagnostic s)
getDiagnostic = (registerDiagnostic .) . unsafe_getDiagnostic

-- CXDiagnosticSet clang_getDiagnosticSetFromTU(CXTranslationUnit Unit);
{# fun clang_getDiagnosticSetFromTU { `Ptr ()' } -> `Ptr ()' id #}
unsafe_getDiagnosticSetFromTU :: TranslationUnit s -> IO (DiagnosticSet ())
unsafe_getDiagnosticSetFromTU t = do
  set <- clang_getDiagnosticSetFromTU (unTranslationUnit t)
  return (mkDiagnosticSet set)

getDiagnosticSetFromTU :: ClangBase m => TranslationUnit s' -> ClangT s m (DiagnosticSet s)
getDiagnosticSetFromTU = registerDiagnosticSet . unsafe_getDiagnosticSetFromTU

-- enum CXDiagnosticDisplayOptions {
--   CXDiagnostic_DisplaySourceLocation = 0x01,
--   CXDiagnostic_DisplayColumn = 0x02,
--   CXDiagnostic_DisplaySourceRanges = 0x04,
--   CXDiagnostic_DisplayOption = 0x08,
--   CXDiagnostic_DisplayCategoryId = 0x10,
--   CXDiagnostic_DisplayCategoryName = 0x20
-- };

-- | Options for rendering of 'Diagnostic' values.
#c
enum DisplayOptions
 { DisplaySourceLocation = CXDiagnostic_DisplaySourceLocation
 , DisplayColumn         = CXDiagnostic_DisplayColumn
 , DisplaySourceRanges   = CXDiagnostic_DisplaySourceRanges
 , DisplayOption         = CXDiagnostic_DisplayOption
 , DisplayCategoryId     = CXDiagnostic_DisplayCategoryId
 , DisplayCategoryName   = CXDiagnostic_DisplayCategoryName
 };
#endc
{#enum DisplayOptions {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags DisplayOptions where
  toBit DisplaySourceLocation = 0x1
  toBit DisplayColumn         = 0x2
  toBit DisplaySourceRanges   = 0x4
  toBit DisplayOption         = 0x8
  toBit DisplayCategoryId     = 0x10
  toBit DisplayCategoryName   = 0x20

-- CXString clang_formatDiagnostic(CXDiagnostic Diagnostic, unsigned Options);
{# fun wrapped_clang_formatDiagnostic as clang_formatDiagnostic { `Ptr ()' , `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_formatDiagnostic :: Diagnostic s -> Int -> IO (ClangString ())
unsafe_formatDiagnostic d i = clang_formatDiagnostic (unDiagnostic d) (fromIntegral i) >>= peek

formatDiagnostic :: ClangBase m => Diagnostic s' -> Int -> ClangT s m (ClangString s)
formatDiagnostic = (registerClangString .) . unsafe_formatDiagnostic

-- unsigned clang_defaultDiagnosticDisplayOptions(void);
{# fun clang_defaultDiagnosticDisplayOptions { } -> `CUInt' #}
defaultDiagnosticDisplayOptions :: IO Int
defaultDiagnosticDisplayOptions = clang_defaultDiagnosticDisplayOptions >>= return . fromIntegral

-- clang_getDiagnosticSeverity(CXDiagnostic);
{# fun clang_getDiagnosticSeverity {id `Ptr ()' } -> `CInt' #}
getDiagnosticSeverity :: Diagnostic s -> IO Severity
getDiagnosticSeverity d = clang_getDiagnosticSeverity (unDiagnostic d) >>= return . toEnum . fromIntegral

-- CXSourceLocation clang_getDiagnosticLocation(CXDiagnostic);
{# fun wrapped_clang_getDiagnosticLocation as clang_getDiagnosticLocation {id `Ptr ()' } -> `Ptr (SourceLocation s)' castPtr #}
getDiagnosticLocation :: Proxy s -> Diagnostic s' -> IO (SourceLocation s)
getDiagnosticLocation _ d = clang_getDiagnosticLocation (unDiagnostic d) >>= peek

-- CXString clang_getDiagnosticSpelling(CXDiagnostic);
{# fun wrapped_clang_getDiagnosticSpelling as clang_getDiagnosticSpelling  { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getDiagnosticSpelling :: Diagnostic s -> IO (ClangString ())
unsafe_getDiagnosticSpelling d = clang_getDiagnosticSpelling (unDiagnostic d) >>= peek

getDiagnosticSpelling :: ClangBase m => Diagnostic s' -> ClangT s m (ClangString s)
getDiagnosticSpelling = registerClangString . unsafe_getDiagnosticSpelling

-- CXString clang_getDiagnosticOption(CXDiagnostic Diag,
--                                                   CXString *Disable);
{# fun wrapped_clang_getDiagnosticOption as clang_getDiagnosticOption { id `Ptr ()', id `Ptr ()' } -> `Ptr ()' id #}
unsafe_getDiagnosticOption :: Diagnostic s -> IO (ClangString (), ClangString ())
unsafe_getDiagnosticOption d =
  alloca (\(disableCXStringPtr :: (Ptr (ClangString ()))) -> do
    diagnosticOptionPtr <- clang_getDiagnosticOption (unDiagnostic d) (castPtr disableCXStringPtr)
    disableCXString <- peek disableCXStringPtr
    diagnosticOption <- peek (castPtr diagnosticOptionPtr)
    return (diagnosticOption, disableCXString))

getDiagnosticOption :: ClangBase m => Diagnostic s' -> ClangT s m (ClangString s, ClangString s)
getDiagnosticOption d = do
  (a, b) <- liftIO $ unsafe_getDiagnosticOption d
  (,) <$> registerClangString (return a) <*> registerClangString (return b)

-- unsigned clang_getDiagnosticCategory(CXDiagnostic);
{# fun clang_getDiagnosticCategory { id `Ptr ()' } -> `Int' #}
getDiagnosticCategory :: Diagnostic s -> IO Int
getDiagnosticCategory d = clang_getDiagnosticCategory (unDiagnostic d)

-- CXString clang_getDiagnosticCategoryText(CXDiagnostic);
{# fun wrapped_clang_getDiagnosticCategoryText as clang_getDiagnosticCategoryText { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getDiagnosticCategoryText :: Diagnostic s -> IO (ClangString ())
unsafe_getDiagnosticCategoryText d = clang_getDiagnosticCategoryText (unDiagnostic d) >>= peek

getDiagnosticCategoryText :: ClangBase m => Diagnostic s' -> ClangT s m (ClangString s)
getDiagnosticCategoryText = registerClangString . unsafe_getDiagnosticCategoryText

-- unsigned clang_getDiagnosticNumRanges(CXDiagnostic);
{# fun clang_getDiagnosticNumRanges { id `Ptr ()' } -> `Int' #}
getDiagnosticNumRanges :: Diagnostic s -> IO Int
getDiagnosticNumRanges d = clang_getDiagnosticNumRanges (unDiagnostic d)

-- CXSourceRange clang_getDiagnosticRange(CXDiagnostic Diagnostic,
--                                                       unsigned Range);
{# fun wrapped_clang_getDiagnosticRange as clang_getDiagnosticRange { id `Ptr ()', `Int' } -> `Ptr (SourceRange s)' castPtr #}
getDiagnosticRange :: Diagnostic s' -> Int -> IO (SourceRange s)
getDiagnosticRange d i = clang_getDiagnosticRange (unDiagnostic d) i >>= peek

-- unsigned clang_getDiagnosticNumFixIts(CXDiagnostic Diagnostic);
{# fun clang_getDiagnosticNumFixIts { id `Ptr ()' } -> `Int' #}
getDiagnosticNumFixIts :: Diagnostic s -> IO Int
getDiagnosticNumFixIts d = clang_getDiagnosticNumFixIts (unDiagnostic d)

-- CXString clang_getDiagnosticFixIt(CXDiagnostic Diagnostic,
--                                                  unsigned FixIt,
--                                                CXSourceRange *ReplacementRange);
{# fun wrapped_clang_getDiagnosticFixIt as clang_getDiagnosticFixIt { id `Ptr ()', `Int', id `Ptr ()' } -> `Ptr (ClangString())' castPtr #}
unsafe_getDiagnosticFixIt :: Diagnostic s' -> Int -> IO (SourceRange s, ClangString ())
unsafe_getDiagnosticFixIt d i =
  alloca (\(replacementRangePtr :: (Ptr (SourceRange s))) -> do
     clangStringPtr <- clang_getDiagnosticFixIt (unDiagnostic d) i (castPtr replacementRangePtr)
     clangString <- peek clangStringPtr
     replacementRange <- peek replacementRangePtr
     return (replacementRange, clangString))

getDiagnosticFixIt :: ClangBase m => Diagnostic s' -> Int
                   -> ClangT s m (SourceRange s, ClangString s)
getDiagnosticFixIt d i = do
  (r, s) <- liftIO $ unsafe_getDiagnosticFixIt d i
  (r,) <$> registerClangString (return s)

-- CXString
-- clang_getTranslationUnitSpelling(CXTranslationUnit CTUnit);
{# fun wrapped_clang_getTranslationUnitSpelling as clang_getTranslationUnitSpelling { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getTranslationUnitSpelling :: TranslationUnit s -> IO (ClangString ())
unsafe_getTranslationUnitSpelling t = clang_getTranslationUnitSpelling (unTranslationUnit t) >>= peek

getTranslationUnitSpelling :: ClangBase m => TranslationUnit s' -> ClangT s m (ClangString s)
getTranslationUnitSpelling = registerClangString . unsafe_getTranslationUnitSpelling

-- CXTranslationUnit clang_createTranslationUnitFromSourceFile(
--                                          CXIndex CIdx,
--                                          const char *source_filename,
--                                          int num_clang_command_line_args,
--                                    const char * const *clang_command_line_args,
--                                          unsigned num_unsaved_files,
--                                          struct CXUnsavedFile *unsaved_files);
{# fun clang_createTranslationUnitFromSourceFile { id `Ptr ()' , `CString' , `Int' , id `Ptr CString' , `Int' , id `Ptr ()' }  -> `Ptr ()' id #}
unsafe_createTranslationUnitFromSourceFile :: Index s -> String -> Int -> Ptr CString -> Int -> Ptr CUnsavedFile -> IO (TranslationUnit ())
unsafe_createTranslationUnitFromSourceFile i s nas as nufs ufs =
  withCString s (\sPtr -> do
     rPtr <- clang_createTranslationUnitFromSourceFile (unIndex i) sPtr nas as nufs (castPtr ufs)
     return (mkTranslationUnit rPtr))

createTranslationUnitFromSourceFile :: ClangBase m => Index s' -> String -> [String]
                                    -> DV.Vector UnsavedFile -> ClangT s m (TranslationUnit s)
createTranslationUnitFromSourceFile idx sf as ufs =
  registerTranslationUnit $
    withStringList as $ \asPtr asLen ->
      withUnsavedFiles ufs $ \ufsPtr ufsLen ->
        unsafe_createTranslationUnitFromSourceFile idx sf asLen asPtr ufsLen ufsPtr

-- CXTranslationUnit clang_createTranslationUnit(CXIndex,
--                                              const char *ast_filename);
{# fun clang_createTranslationUnit { id `Ptr ()', `CString' } -> `Ptr ()' #}
unsafe_createTranslationUnit :: Index s -> String -> IO (TranslationUnit ())
unsafe_createTranslationUnit i s =
  withCString s (\strPtr -> do
   trPtr <- clang_createTranslationUnit (unIndex i) strPtr
   return (mkTranslationUnit trPtr))

createTranslationUnit :: ClangBase m => Index s' -> String -> ClangT s m (TranslationUnit s)
createTranslationUnit = (registerTranslationUnit .) . unsafe_createTranslationUnit

-- enum CXTranslationUnit_Flags {
--   CXTranslationUnit_None = 0x0,
--   CXTranslationUnit_DetailedPreprocessingRecord = 0x01,
--   CXTranslationUnit_Incomplete = 0x02,
--   CXTranslationUnit_PrecompiledPreamble = 0x04,
--   CXTranslationUnit_CacheCompletionResults = 0x08,
--   CXTranslationUnit_ForSerialization = 0x10,
--   CXTranslationUnit_CXXChainedPCH = 0x20,
--   CXTranslationUnit_SkipFunctionBodies = 0x40,
--   CXTranslationUnit_IncludeBriefCommentsInCodeCompletion = 0x80
-- };

-- | Flags that control how a translation unit is parsed.
--
-- * 'DetailedPreprocessingRecordFlag': Used to indicate that the parser should construct a
--   "detailed" preprocessing record, including all macro definitions and instantiations.
--   Constructing a detailed preprocessing record requires more memory and time to parse,
--   since the information contained in the record is usually not retained. However, it can be
--   useful for applications that require more detailed information about the behavior of the
--   preprocessor.
--
-- * 'IncompleteFlag': Used to indicate that the translation unit is incomplete.
--   When a translation unit is considered "incomplete", semantic analysis that is typically
--   performed at the end of the translation unit will be suppressed. For example, this
--   suppresses the completion of tentative declarations in C and of instantiation of
--   implicitly-instantiation function templates in C++. This option is typically used when
--   parsing a header with the intent of producing a precompiled header.
--
-- * 'PrecompiledPreambleFlag': Used to indicate that the translation unit should be built
--   with an implicit precompiled header for the preamble. An implicit precompiled header is
--   used as an optimization when a particular translation unit is likely to be reparsed many
--   times when the sources aren't changing that often. In this case, an implicit precompiled
--   header will be built containing all of the initial includes at the top of the main file
--   (what we refer to as the "preamble" of the file). In subsequent parses, if the preamble
--   or the files in it have not changed, 'Clang.TranslationUnit.reparse' will re-use the
--   implicit precompiled header to improve parsing performance.
--
-- * 'CacheCompletionResultsFlag': Used to indicate that the translation unit should cache
--   some code-completion results with each reparse of the source file.
--   Caching of code-completion results is a performance optimization that introduces some
--   overhead to reparsing but improves the performance of code-completion operations.
--
-- * 'ForSerializationFlag': Used to indicate that the translation unit will be serialized
--   'Clang.TranslationUnit.save'. This option is typically used when parsing a header with
--   the intent of producing a precompiled header.
--
-- * 'CXXChainedPCHFlag': DEPRECATED: Enabled chained precompiled preambles in C++. Note:
--   this is a *temporary* option that is available only while we are testing C++ precompiled
--   preamble support. It is deprecated.
--
-- * 'SkipFunctionBodiesFlag': Used to indicate that function/method bodies should be skipped
--   while parsing. This option can be used to search for declarations/definitions while
--   ignoring the usages.
--
-- * 'IncludeBriefCommentsInCodeCompletionFlag': Used to indicate that brief documentation
--   comments should be included into the set of code completions returned from this
--   translation unit.
#c
enum TranslationUnitFlags
  { DefaultTranslationUnitFlags              = CXTranslationUnit_None
  , DetailedPreprocessingRecordFlag          = CXTranslationUnit_DetailedPreprocessingRecord
  , IncompleteFlag                           = CXTranslationUnit_Incomplete
  , PrecompiledPreambleFlag                  = CXTranslationUnit_PrecompiledPreamble
  , CacheCompletionResultsFlag               = CXTranslationUnit_CacheCompletionResults
  , ForSerializationFlag                     = CXTranslationUnit_ForSerialization
  , ChainedPCHFlag                           = CXTranslationUnit_CXXChainedPCH
  , SkipFunctionBodiesFlag                   = CXTranslationUnit_SkipFunctionBodies
  , IncludeBriefCommentsInCodeCompletionFlag = CXTranslationUnit_IncludeBriefCommentsInCodeCompletion
 };
#endc
{# enum TranslationUnitFlags {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags TranslationUnitFlags where
  toBit DefaultTranslationUnitFlags              = 0x0
  toBit DetailedPreprocessingRecordFlag          = 0x01
  toBit IncompleteFlag                           = 0x02
  toBit PrecompiledPreambleFlag                  = 0x04
  toBit CacheCompletionResultsFlag               = 0x08
  toBit ForSerializationFlag                     = 0x10
  toBit ChainedPCHFlag                           = 0x20
  toBit SkipFunctionBodiesFlag                   = 0x40
  toBit IncludeBriefCommentsInCodeCompletionFlag = 0x80

-- unsigned clang_defaultEditingTranslationUnitOptions(void);
{# fun clang_defaultEditingTranslationUnitOptions as defaultEditingTranslationUnitOptions {} -> `Int' #}

maybeTranslationUnit :: TranslationUnit s' -> Maybe (TranslationUnit s)
maybeTranslationUnit (TranslationUnit p) | p == nullPtr = Nothing
maybeTranslationUnit f                         = Just (unsafeCoerce f)

-- CXTranslationUnit clang_parseTranslationUnit(CXIndex CIdx,
--                                                     const char *source_filename,
--                                          const char * const *command_line_args,
--                                                       int num_command_line_args,
--                                             struct CXUnsavedFile *unsaved_files,
--                                                      unsigned num_unsaved_files,
--                                                             unsigned options);
{# fun clang_parseTranslationUnit { id `Ptr ()', `CString' , id `Ptr CString', `Int', id `Ptr ()', `Int', `Int'}  -> `Ptr ()' id #}
unsafe_parseTranslationUnit :: Index s -> CString -> Ptr CString -> Int -> Ptr CUnsavedFile -> Int -> Int -> IO (Maybe (TranslationUnit ()))
unsafe_parseTranslationUnit i s as nas ufs nufs o =
  clang_parseTranslationUnit (unIndex i) s as nas (castPtr ufs) nufs o >>= return . maybeTranslationUnit . mkTranslationUnit

parseTranslationUnit :: ClangBase m => Index s' -> Maybe String -> [String]
                     -> DV.Vector UnsavedFile -> Int -> ClangT s m (Maybe (TranslationUnit s))
parseTranslationUnit idx maySF as ufs opts = do
    mayTU <- liftIO $
      withMaybeCString maySF $ \cSF ->
        withStringList as $ \asPtr asLen ->
          withUnsavedFiles ufs $ \ufsPtr ufsLen ->
            unsafe_parseTranslationUnit idx cSF asPtr asLen ufsPtr ufsLen opts
    case mayTU of
      Just tu -> Just <$> registerTranslationUnit (return tu)
      Nothing -> return Nothing

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString (Just s) f = withCString s f
withMaybeCString Nothing  f = f nullPtr

withStringList :: [String] -> (Ptr CString -> Int -> IO a) -> IO a
withStringList [] f = f nullPtr 0
withStringList strs f = do
    let len = length strs
    allocaArray len $ \arr -> go arr len arr strs
  where
    go arr len _ [] = f arr len
    go arr len ptr (s : ss) = withCString s $ \cs -> do
      poke ptr cs
      go arr len (advancePtr ptr 1) ss

-- enum CXSaveTranslationUnit_Flags {
--   CXSaveTranslationUnit_None = 0x0
-- };

-- | Flags that control how a translation unit is saved.
#c
enum SaveTranslationUnitFlags
  {DefaultSaveTranslationUnitFlags = CXSaveTranslationUnit_None};
#endc
{# enum SaveTranslationUnitFlags {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}
instance BitFlags SaveTranslationUnitFlags where
  toBit DefaultSaveTranslationUnitFlags = 0x0

-- unsigned clang_defaultSaveOptions(CXTranslationUnit TU);
{# fun clang_defaultSaveOptions { id `Ptr ()' } -> `Int' #}
defaultSaveOptions :: TranslationUnit s -> IO Int
defaultSaveOptions t = clang_defaultSaveOptions (unTranslationUnit t)


-- int clang_saveTranslationUnit(CXTranslationUnit TU,
--                                              const char *FileName,
--                                              unsigned options);
{# fun clang_saveTranslationUnit { id `Ptr ()' , `CString', `Int' } -> `Int' #}
saveTranslationUnit :: TranslationUnit s -> String -> Int -> IO Bool
saveTranslationUnit t s i =
 withCString s (\sPtr -> do
   r <- clang_saveTranslationUnit (unTranslationUnit t) sPtr i
   return (toBool ((if (r /= 0) then 0 else 1) :: Int)))

-- enum CXReparse_Flags {
--   CXReparse_None = 0x0
-- };

-- | Flags that control how a translation unit is reparsed.
#c
enum ReparseFlags {
  DefaultReparseFlags = CXReparse_None
};
#endc
{# enum ReparseFlags {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags ReparseFlags where
  toBit DefaultReparseFlags = 0x0

-- unsigned clang_defaultReparseOptions(CXTranslationUnit TU);
{# fun clang_defaultReparseOptions { id `Ptr ()' } -> `CUInt' #}
defaultReparseOptions :: TranslationUnit s -> IO Int
defaultReparseOptions t = clang_defaultReparseOptions (unTranslationUnit t) >>= return . fromIntegral

-- int clang_reparseTranslationUnit(CXTranslationUnit TU,
--                                                 unsigned num_unsaved_files,
--                                           struct CXUnsavedFile *unsaved_files,
--                                                 unsigned options);
{# fun clang_reparseTranslationUnit { id `Ptr ()', `Int' , id `Ptr ()' , `Int' } -> `Bool' toBool #}
unsafe_reparseTranslationUnit :: TranslationUnit s -> Ptr CUnsavedFile -> Int -> Int -> IO Bool
unsafe_reparseTranslationUnit t ufs nufs i =
  clang_reparseTranslationUnit (unTranslationUnit t) nufs (castPtr ufs) i

reparseTranslationUnit :: ClangBase m => TranslationUnit s' -> DV.Vector UnsavedFile -> Int
                       -> ClangT s m Bool
reparseTranslationUnit tu ufs opts = liftIO $
  withUnsavedFiles ufs $ \ufsPtr ufsLen ->
    unsafe_reparseTranslationUnit tu ufsPtr ufsLen opts


-- enum CXCursorKind {
--   /* Declarations */
--   /**
--    * \brief A declaration whose specific kind is not exposed via this
--    * interface.
--    *
--    * Unexposed declarations have the same operations as any other kind
--    * of declaration; one can extract their location information,
--    * spelling, find their definitions, etc. However, the specific kind
--    * of the declaration is not reported.
--    */
--   CXCursor_UnexposedDecl                 = 1,
--   /** \brief A C or C++ struct. */
--   CXCursor_StructDecl                    = 2,
--   /** \brief A C or C++ union. */
--   CXCursor_UnionDecl                     = 3,
--   /** \brief A C++ class. */
--   CXCursor_ClassDecl                     = 4,
--   /** \brief An enumeration. */
--   CXCursor_EnumDecl                      = 5,
--   /**
--    * \brief A field (in C) or non-static data member (in C++) in a
--    * struct, union, or C++ class.
--    */
--   CXCursor_FieldDecl                     = 6,
--   /** \brief An enumerator constant. */
--   CXCursor_EnumConstantDecl              = 7,
--   /** \brief A function. */
--   CXCursor_FunctionDecl                  = 8,
--   /** \brief A variable. */
--   CXCursor_VarDecl                       = 9,
--   /** \brief A function or method parameter. */
--   CXCursor_ParmDecl                      = 10,
--   /** \brief An Objective-C @interface. */
--   CXCursor_ObjCInterfaceDecl             = 11,
--   /** \brief An Objective-C @interface for a category. */
--   CXCursor_ObjCCategoryDecl              = 12,
--   /** \brief An Objective-C @protocol declaration. */
--   CXCursor_ObjCProtocolDecl              = 13,
--   /** \brief An Objective-C @property declaration. */
--   CXCursor_ObjCPropertyDecl              = 14,
--   /** \brief An Objective-C instance variable. */
--   CXCursor_ObjCIvarDecl                  = 15,
--   /** \brief An Objective-C instance method. */
--   CXCursor_ObjCInstanceMethodDecl        = 16,
--   /** \brief An Objective-C class method. */
--   CXCursor_ObjCClassMethodDecl           = 17,
--   /** \brief An Objective-C @implementation. */
--   CXCursor_ObjCImplementationDecl        = 18,
--   /** \brief An Objective-C @implementation for a category. */
--   CXCursor_ObjCCategoryImplDecl          = 19,
--   /** \brief A typedef */
--   CXCursor_TypedefDecl                   = 20,
--   /** \brief A C++ class method. */
--   CXCursor_CXXMethod                     = 21,
--   /** \brief A C++ namespace. */
--   CXCursor_Namespace                     = 22,
--   /** \brief A linkage specification, e.g. 'extern "C"'. */
--   CXCursor_LinkageSpec                   = 23,
--   /** \brief A C++ constructor. */
--   CXCursor_Constructor                   = 24,
--   /** \brief A C++ destructor. */
--   CXCursor_Destructor                    = 25,
--   /** \brief A C++ conversion function. */
--   CXCursor_ConversionFunction            = 26,
--   /** \brief A C++ template type parameter. */
--   CXCursor_TemplateTypeParameter         = 27,
--   /** \brief A C++ non-type template parameter. */
--   CXCursor_NonTypeTemplateParameter      = 28,
--   /** \brief A C++ template template parameter. */
--   CXCursor_TemplateTemplateParameter     = 29,
--   /** \brief A C++ function template. */
--   CXCursor_FunctionTemplate              = 30,
--   /** \brief A C++ class template. */
--   CXCursor_ClassTemplate                 = 31,
--   /** \brief A C++ class template partial specialization. */
--   CXCursor_ClassTemplatePartialSpecialization = 32,
--   /** \brief A C++ namespace alias declaration. */
--   CXCursor_NamespaceAlias                = 33,
--   /** \brief A C++ using directive. */
--   CXCursor_UsingDirective                = 34,
--   /** \brief A C++ using declaration. */
--   CXCursor_UsingDeclaration              = 35,
--   /** \brief A C++ alias declaration */
--   CXCursor_TypeAliasDecl                 = 36,
--   /** \brief An Objective-C @synthesize definition. */
--   CXCursor_ObjCSynthesizeDecl            = 37,
--   /** \brief An Objective-C @dynamic definition. */
--   CXCursor_ObjCDynamicDecl               = 38,
--   /** \brief An access specifier. */
--   CXCursor_CXXAccessSpecifier            = 39,

--   CXCursor_FirstDecl                     = CXCursor_UnexposedDecl,
--   CXCursor_LastDecl                      = CXCursor_CXXAccessSpecifier,

--   /* References */
--   CXCursor_FirstRef                      = 40, /* Decl references */
--   CXCursor_ObjCSuperClassRef             = 40,
--   CXCursor_ObjCProtocolRef               = 41,
--   CXCursor_ObjCClassRef                  = 42,
--   /**
--    * \brief A reference to a type declaration.
--    *
--    * A type reference occurs anywhere where a type is named but not
--    * declared. For example, given:
--    *
--    * \code
--    * typedef unsigned size_type;
--    * size_type size;
--    * \endcode
--    *
--    * The typedef is a declaration of size_type (CXCursor_TypedefDecl),
--    * while the type of the variable "size" is referenced. The cursor
--    * referenced by the type of size is the typedef for size_type.
--    */
--   CXCursor_TypeRef                       = 43,
--   CXCursor_CXXBaseSpecifier              = 44,
--   /**
--    * \brief A reference to a class template, function template, template
--    * template parameter, or class template partial specialization.
--    */
--   CXCursor_TemplateRef                   = 45,
--   /**
--    * \brief A reference to a namespace or namespace alias.
--    */
--   CXCursor_NamespaceRef                  = 46,
--   /**
--    * \brief A reference to a member of a struct, union, or class that occurs in
--    * some non-expression context, e.g., a designated initializer.
--    */
--   CXCursor_MemberRef                     = 47,
--   /**
--    * \brief A reference to a labeled statement.
--    *
--    * This cursor kind is used to describe the jump to "start_over" in the
--    * goto statement in the following example:
--    *
--    * \code
--    *   start_over:
--    *     ++counter;
--    *
--    *     goto start_over;
--    * \endcode
--    *
--    * A label reference cursor refers to a label statement.
--    */
--   CXCursor_LabelRef                      = 48,

--   /**
--    * \brief A reference to a set of overloaded functions or function templates
--    * that has not yet been resolved to a specific function or function template.
--    *
--    * An overloaded declaration reference cursor occurs in C++ templates where
--    * a dependent name refers to a function. For example:
--    *
--    * \code
--    * template<typename T> void swap(T&, T&);
--    *
--    * struct X { ... };
--    * void swap(X&, X&);
--    *
--    * template<typename T>
--    * void reverse(T* first, T* last) {
--    *   while (first < last - 1) {
--    *     swap(*first, *--last);
--    *     ++first;
--    *   }
--    * }
--    *
--    * struct Y { };
--    * void swap(Y&, Y&);
--    * \endcode
--    *
--    * Here, the identifier "swap" is associated with an overloaded declaration
--    * reference. In the template definition, "swap" refers to either of the two
--    * "swap" functions declared above, so both results will be available. At
--    * instantiation time, "swap" may also refer to other functions found via
--    * argument-dependent lookup (e.g., the "swap" function at the end of the
--    * example).
--    *
--    * The functions \c clang_getNumOverloadedDecls() and
--    * \c clang_getOverloadedDecl() can be used to retrieve the definitions
--    * referenced by this cursor.
--    */
--   CXCursor_OverloadedDeclRef             = 49,

--   /*
--    * \brief A reference to a variable that occurs in some non-expression
--    * context, e.g., a C++ lambda capture list.
--    */
--   CXCursor_VariableRef                   = 50,

--   CXCursor_LastRef                       = CXCursor_VariableRef,

--   /* Error conditions */
--   CXCursor_FirstInvalid                  = 70,
--   CXCursor_InvalidFile                   = 70,
--   CXCursor_NoDeclFound                   = 71,
--   CXCursor_NotImplemented                = 72,
--   CXCursor_InvalidCode                   = 73,
--   CXCursor_LastInvalid                   = CXCursor_InvalidCode,

--   /* Expressions */
--   CXCursor_FirstExpr                     = 100,

--   /**
--    * \brief An expression whose specific kind is not exposed via this
--    * interface.
--    *
--    * Unexposed expressions have the same operations as any other kind
--    * of expression; one can extract their location information,
--    * spelling, children, etc. However, the specific kind of the
--    * expression is not reported.
--    */
--   CXCursor_UnexposedExpr                 = 100,

--   /**
--    * \brief An expression that refers to some value declaration, such
--    * as a function, varible, or enumerator.
--    */
--   CXCursor_DeclRefExpr                   = 101,

--   /**
--    * \brief An expression that refers to a member of a struct, union,
--    * class, Objective-C class, etc.
--    */
--   CXCursor_MemberRefExpr                 = 102,

--   /** \brief An expression that calls a function. */
--   CXCursor_CallExpr                      = 103,

--   /** \brief An expression that sends a message to an Objective-C
--    object or class. */
--   CXCursor_ObjCMessageExpr               = 104,

--   /** \brief An expression that represents a block literal. */
--   CXCursor_BlockExpr                     = 105,

--   /** \brief An integer literal.
--    */
--   CXCursor_IntegerLiteral                = 106,

--   /** \brief A floating point number literal.
--    */
--   CXCursor_FloatingLiteral               = 107,

--   /** \brief An imaginary number literal.
--    */
--   CXCursor_ImaginaryLiteral              = 108,

--   /** \brief A string literal.
--    */
--   CXCursor_StringLiteral                 = 109,

--   /** \brief A character literal.
--    */
--   CXCursor_CharacterLiteral              = 110,

--   /** \brief A parenthesized expression, e.g. "(1)".
--    *
--    * This AST node is only formed if full location information is requested.
--    */
--   CXCursor_ParenExpr                     = 111,

--   /** \brief This represents the unary-expression's (except sizeof and
--    * alignof).
--    */
--   CXCursor_UnaryOperator                 = 112,

--   /** \brief [C99 6.5.2.1] Array Subscripting.
--    */
--   CXCursor_ArraySubscriptExpr            = 113,

--   /** \brief A builtin binary operation expression such as "x + y" or
--    * "x <= y".
--    */
--   CXCursor_BinaryOperator                = 114,

--   /** \brief Compound assignment such as "+=".
--    */
--   CXCursor_CompoundAssignOperator        = 115,

--   /** \brief The ?: ternary operator.
--    */
--   CXCursor_ConditionalOperator           = 116,

--   /** \brief An explicit cast in C (C99 6.5.4) or a C-style cast in C++
--    * (C++ [expr.cast]), which uses the syntax (Type)expr.
--    *
--    * For example: (int)f.
--    */
--   CXCursor_CStyleCastExpr                = 117,

--   /** \brief [C99 6.5.2.5]
--    */
--   CXCursor_CompoundLiteralExpr           = 118,

--   /** \brief Describes an C or C++ initializer list.
--    */
--   CXCursor_InitListExpr                  = 119,

--   /** \brief The GNU address of label extension, representing &&label.
--    */
--   CXCursor_AddrLabelExpr                 = 120,

--   /** \brief This is the GNU Statement Expression extension: ({int X=4; X;})
--    */
--   CXCursor_StmtExpr                      = 121,

--   /** \brief Represents a C1X generic selection.
--    */
--   CXCursor_GenericSelectionExpr          = 122,

--   /** \brief Implements the GNU __null extension, which is a name for a null
--    * pointer constant that has integral type (e.g., int or long) and is the same
--    * size and alignment as a pointer.
--    *
--    * The __null extension is typically only used by system headers, which define
--    * NULL as __null in C++ rather than using 0 (which is an integer that may not
--    * match the size of a pointer).
--    */
--   CXCursor_GNUNullExpr                   = 123,

--   /** \brief C++'s static_cast<> expression.
--    */
--   CXCursor_CXXStaticCastExpr             = 124,

--   /** \brief C++'s dynamic_cast<> expression.
--    */
--   CXCursor_CXXDynamicCastExpr            = 125,

--   /** \brief C++'s reinterpret_cast<> expression.
--    */
--   CXCursor_CXXReinterpretCastExpr        = 126,

--   /** \brief C++'s const_cast<> expression.
--    */
--   CXCursor_CXXConstCastExpr              = 127,

--   /** \brief Represents an explicit C++ type conversion that uses "functional"
--    * notion (C++ [expr.type.conv]).
--    *
--    * Example:
--    * \code
--    *   x = int(0.5);
--    * \endcode
--    */
--   CXCursor_CXXFunctionalCastExpr         = 128,

--   /** \brief A C++ typeid expression (C++ [expr.typeid]).
--    */
--   CXCursor_CXXTypeidExpr                 = 129,

--   /** \brief [C++ 2.13.5] C++ Boolean Literal.
--    */
--   CXCursor_CXXBoolLiteralExpr            = 130,

--   /** \brief [C++0x 2.14.7] C++ Pointer Literal.
--    */
--   CXCursor_CXXNullPtrLiteralExpr         = 131,

--   /** \brief Represents the "this" expression in C++
--    */
--   CXCursor_CXXThisExpr                   = 132,

--   /** \brief [C++ 15] C++ Throw Expression.
--    *
--    * This handles 'throw' and 'throw' assignment-expression. When
--    * assignment-expression isn't present, Op will be null.
--    */
--   CXCursor_CXXThrowExpr                  = 133,

--   /** \brief A new expression for memory allocation and constructor calls, e.g:
--    * "new CXXNewExpr(foo)".
--    */
--   CXCursor_CXXNewExpr                    = 134,

--   /** \brief A delete expression for memory deallocation and destructor calls,
--    * e.g. "delete[] pArray".
--    */
--   CXCursor_CXXDeleteExpr                 = 135,

--   /** \brief A unary expression.
--    */
--   CXCursor_UnaryExpr                     = 136,

--   /** \brief ObjCStringLiteral, used for Objective-C string literals i.e. "foo".
--    */
--   CXCursor_ObjCStringLiteral             = 137,

--   /** \brief ObjCEncodeExpr, used for in Objective-C.
--    */
--   CXCursor_ObjCEncodeExpr                = 138,

--   /** \brief ObjCSelectorExpr used for in Objective-C.
--    */
--   CXCursor_ObjCSelectorExpr              = 139,

--   /** \brief Objective-C's protocol expression.
--    */
--   CXCursor_ObjCProtocolExpr              = 140,

--   /** \brief An Objective-C "bridged" cast expression, which casts between
--    * Objective-C pointers and C pointers, transferring ownership in the process.
--    *
--    * \code
--    *   NSString *str = (__bridge_transfer NSString *)CFCreateString();
--    * \endcode
--    */
--   CXCursor_ObjCBridgedCastExpr           = 141,

--   /** \brief Represents a C++0x pack expansion that produces a sequence of
--    * expressions.
--    *
--    * A pack expansion expression contains a pattern (which itself is an
--    * expression) followed by an ellipsis. For example:
--    *
--    * \code
--    * template<typename F, typename ...Types>
--    * void forward(F f, Types &&...args) {
--    *  f(static_cast<Types&&>(args)...);
--    * }
--    * \endcode
--    */
--   CXCursor_PackExpansionExpr             = 142,

--   /** \brief Represents an expression that computes the length of a parameter
--    * pack.
--    *
--    * \code
--    * template<typename ...Types>
--    * struct count {
--    *   static const unsigned value = sizeof...(Types);
--    * };
--    * \endcode
--    */
--   CXCursor_SizeOfPackExpr                = 143,

--   /* \brief Represents a C++ lambda expression that produces a local function
--    * object.
--    *
--    * \code
--    * void abssort(float *x, unsigned N) {
--    *   std::sort(x, x + N,
--    *             [](float a, float b) {
--    *               return std::abs(a) < std::abs(b);
--    *             });
--    * }
--    * \endcode
--    */
--   CXCursor_LambdaExpr                    = 144,

--   /** \brief Objective-c Boolean Literal.
--    */
--   CXCursor_ObjCBoolLiteralExpr           = 145,

--   /** \brief Represents the "self" expression in a ObjC method.
--    */
--   CXCursor_ObjCSelfExpr                  = 146,

--   CXCursor_LastExpr                      = CXCursor_ObjCSelfExpr,

--   /* Statements */
--   CXCursor_FirstStmt                     = 200,
--   /**
--    * \brief A statement whose specific kind is not exposed via this
--    * interface.
--    *
--    * Unexposed statements have the same operations as any other kind of
--    * statement; one can extract their location information, spelling,
--    * children, etc. However, the specific kind of the statement is not
--    * reported.
--    */
--   CXCursor_UnexposedStmt                 = 200,

--   /** \brief A labelled statement in a function.
--    *
--    * This cursor kind is used to describe the "start_over:" label statement in
--    * the following example:
--    *
--    * \code
--    *   start_over:
--    *     ++counter;
--    * \endcode
--    *
--    */
--   CXCursor_LabelStmt                     = 201,

--   /** \brief A group of statements like { stmt stmt }.
--    *
--    * This cursor kind is used to describe compound statements, e.g. function
--    * bodies.
--    */
--   CXCursor_CompoundStmt                  = 202,

--   /** \brief A case statment.
--    */
--   CXCursor_CaseStmt                      = 203,

--   /** \brief A default statement.
--    */
--   CXCursor_DefaultStmt                   = 204,

--   /** \brief An if statement
--    */
--   CXCursor_IfStmt                        = 205,

--   /** \brief A switch statement.
--    */
--   CXCursor_SwitchStmt                    = 206,

--   /** \brief A while statement.
--    */
--   CXCursor_WhileStmt                     = 207,

--   /** \brief A do statement.
--    */
--   CXCursor_DoStmt                        = 208,

--   /** \brief A for statement.
--    */
--   CXCursor_ForStmt                       = 209,

--   /** \brief A goto statement.
--    */
--   CXCursor_GotoStmt                      = 210,

--   /** \brief An indirect goto statement.
--    */
--   CXCursor_IndirectGotoStmt              = 211,

--   /** \brief A continue statement.
--    */
--   CXCursor_ContinueStmt                  = 212,

--   /** \brief A break statement.
--    */
--   CXCursor_BreakStmt                     = 213,

--   /** \brief A return statement.
--    */
--   CXCursor_ReturnStmt                    = 214,

--   /** \brief A GNU inline assembly statement extension.
--    */
--   CXCursor_GCCAsmStmt                    = 215,
--   CXCursor_AsmStmt                       = CXCursor_GCCAsmStmt,

--   /** \brief Objective-C's overall @try-@catc-@finall statement.
--    */
--   CXCursor_ObjCAtTryStmt                 = 216,

--   /** \brief Objective-C's @catch statement.
--    */
--   CXCursor_ObjCAtCatchStmt               = 217,

--   /** \brief Objective-C's @finally statement.
--    */
--   CXCursor_ObjCAtFinallyStmt             = 218,

--   /** \brief Objective-C's @throw statement.
--    */
--   CXCursor_ObjCAtThrowStmt               = 219,

--   /** \brief Objective-C's @synchronized statement.
--    */
--   CXCursor_ObjCAtSynchronizedStmt        = 220,

--   /** \brief Objective-C's autorelease pool statement.
--    */
--   CXCursor_ObjCAutoreleasePoolStmt       = 221,

--   /** \brief Objective-C's collection statement.
--    */
--   CXCursor_ObjCForCollectionStmt         = 222,

--   /** \brief C++'s catch statement.
--    */
--   CXCursor_CXXCatchStmt                  = 223,

--   /** \brief C++'s try statement.
--    */
--   CXCursor_CXXTryStmt                    = 224,

--   /** \brief C++'s for (* : *) statement.
--    */
--   CXCursor_CXXForRangeStmt               = 225,

--   /** \brief Windows Structured Exception Handling's try statement.
--    */
--   CXCursor_SEHTryStmt                    = 226,

--   /** \brief Windows Structured Exception Handling's except statement.
--    */
--   CXCursor_SEHExceptStmt                 = 227,

--   /** \brief Windows Structured Exception Handling's finally statement.
--    */
--   CXCursor_SEHFinallyStmt                = 228,

--   /** \brief A MS inline assembly statement extension.
--    */
--   CXCursor_MSAsmStmt                     = 229,

--   /** \brief The null satement ";": C99 6.8.3p3.
--    *
--    * This cursor kind is used to describe the null statement.
--    */
--   CXCursor_NullStmt                      = 230,

--   /** \brief Adaptor class for mixing declarations with statements and
--    * expressions.
--    */
--   CXCursor_DeclStmt                      = 231,

--   /** \brief OpenMP parallel directive.
--    */
--   CXCursor_OMPParallelDirective          = 232,

--   CXCursor_LastStmt                      = CXCursor_OMPParallelDirective,

--   /**
--    * \brief Cursor that represents the translation unit itself.
--    *
--    * The translation unit cursor exists primarily to act as the root
--    * cursor for traversing the contents of a translation unit.
--    */
--   CXCursor_TranslationUnit               = 300,

--   /* Attributes */
--   CXCursor_FirstAttr                     = 400,
--   /**
--    * \brief An attribute whose specific kind is not exposed via this
--    * interface.
--    */
--   CXCursor_UnexposedAttr                 = 400,

--   CXCursor_IBActionAttr                  = 401,
--   CXCursor_IBOutletAttr                  = 402,
--   CXCursor_IBOutletCollectionAttr        = 403,
--   CXCursor_CXXFinalAttr                  = 404,
--   CXCursor_CXXOverrideAttr               = 405,
--   CXCursor_AnnotateAttr                  = 406,
--   CXCursor_AsmLabelAttr                  = 407,
--   CXCursor_PackedAttr                    = 408,
--   CXCursor_LastAttr                      = CXCursor_PackedAttr,

--   /* Preprocessing */
--   CXCursor_PreprocessingDirective        = 500,
--   CXCursor_MacroDefinition               = 501,
--   CXCursor_MacroExpansion                = 502,
--   CXCursor_MacroInstantiation            = CXCursor_MacroExpansion,
--   CXCursor_InclusionDirective            = 503,
--   CXCursor_FirstPreprocessing            = CXCursor_PreprocessingDirective,
--   CXCursor_LastPreprocessing             = CXCursor_InclusionDirective

--   /* Extra Declarations */
--   /**
--    * \brief A module import declaration.
--    */
--   CXCursor_ModuleImportDecl              = 600,
--   CXCursor_FirstExtraDecl                = CXCursor_ModuleImportDecl,
--   CXCursor_LastExtraDecl                 = CXCursor_ModuleImportDecl
-- };
#c
enum CursorKind
  { UnexposedDeclCursor                      = CXCursor_UnexposedDecl
  , StructDeclCursor                         = CXCursor_StructDecl
  , UnionDeclCursor                          = CXCursor_UnionDecl
  , ClassDeclCursor                          = CXCursor_ClassDecl
  , EnumDeclCursor                           = CXCursor_EnumDecl
  , FieldDeclCursor                          = CXCursor_FieldDecl
  , EnumConstantDeclCursor                   = CXCursor_EnumConstantDecl
  , FunctionDeclCursor                       = CXCursor_FunctionDecl
  , VarDeclCursor                            = CXCursor_VarDecl
  , ParmDeclCursor                           = CXCursor_ParmDecl
  , ObjCInterfaceDeclCursor                  = CXCursor_ObjCInterfaceDecl
  , ObjCCategoryDeclCursor                   = CXCursor_ObjCCategoryDecl
  , ObjCProtocolDeclCursor                   = CXCursor_ObjCProtocolDecl
  , ObjCPropertyDeclCursor                   = CXCursor_ObjCPropertyDecl
  , ObjCIvarDeclCursor                       = CXCursor_ObjCIvarDecl
  , ObjCInstanceMethodDeclCursor             = CXCursor_ObjCInstanceMethodDecl
  , ObjCClassMethodDeclCursor                = CXCursor_ObjCClassMethodDecl
  , ObjCImplementationDeclCursor             = CXCursor_ObjCImplementationDecl
  , ObjCCategoryImplDeclCursor               = CXCursor_ObjCCategoryImplDecl
  , TypedefDeclCursor                        = CXCursor_TypedefDecl
  , CXXMethodCursor                          = CXCursor_CXXMethod
  , NamespaceCursor                          = CXCursor_Namespace
  , LinkageSpecCursor                        = CXCursor_LinkageSpec
  , ConstructorCursor                        = CXCursor_Constructor
  , DestructorCursor                         = CXCursor_Destructor
  , ConversionFunctionCursor                 = CXCursor_ConversionFunction
  , TemplateTypeParameterCursor              = CXCursor_TemplateTypeParameter
  , NonTypeTemplateParameterCursor           = CXCursor_NonTypeTemplateParameter
  , TemplateTemplateParameterCursor          = CXCursor_TemplateTemplateParameter
  , FunctionTemplateCursor                   = CXCursor_FunctionTemplate
  , ClassTemplateCursor                      = CXCursor_ClassTemplate
  , ClassTemplatePartialSpecializationCursor = CXCursor_ClassTemplatePartialSpecialization
  , NamespaceAliasCursor                     = CXCursor_NamespaceAlias
  , UsingDirectiveCursor                     = CXCursor_UsingDirective
  , UsingDeclarationCursor                   = CXCursor_UsingDeclaration
  , TypeAliasDeclCursor                      = CXCursor_TypeAliasDecl
  , ObjCSynthesizeDeclCursor                 = CXCursor_ObjCSynthesizeDecl
  , ObjCDynamicDeclCursor                    = CXCursor_ObjCDynamicDecl
  , CXXAccessSpecifierCursor                 = CXCursor_CXXAccessSpecifier
  , ObjCSuperClassRefCursor                  = CXCursor_ObjCSuperClassRef
  , ObjCProtocolRefCursor                    = CXCursor_ObjCProtocolRef
  , ObjCClassRefCursor                       = CXCursor_ObjCClassRef
  , TypeRefCursor                            = CXCursor_TypeRef
  , CXXBaseSpecifierCursor                   = CXCursor_CXXBaseSpecifier
  , TemplateRefCursor                        = CXCursor_TemplateRef
  , NamespaceRefCursor                       = CXCursor_NamespaceRef
  , MemberRefCursor                          = CXCursor_MemberRef
  , LabelRefCursor                           = CXCursor_LabelRef
  , OverloadedDeclRefCursor                  = CXCursor_OverloadedDeclRef
  , VariableRefCursor                        = CXCursor_VariableRef
  , InvalidFileCursor                        = CXCursor_InvalidFile
  , NoDeclFoundCursor                        = CXCursor_NoDeclFound
  , NotImplementedCursor                     = CXCursor_NotImplemented
  , InvalidCodeCursor                        = CXCursor_InvalidCode
  , UnexposedExprCursor                      = CXCursor_UnexposedExpr
  , DeclRefExprCursor                        = CXCursor_DeclRefExpr
  , MemberRefExprCursor                      = CXCursor_MemberRefExpr
  , CallExprCursor                           = CXCursor_CallExpr
  , ObjCMessageExprCursor                    = CXCursor_ObjCMessageExpr
  , BlockExprCursor                          = CXCursor_BlockExpr
  , IntegerLiteralCursor                     = CXCursor_IntegerLiteral
  , FloatingLiteralCursor                    = CXCursor_FloatingLiteral
  , ImaginaryLiteralCursor                   = CXCursor_ImaginaryLiteral
  , StringLiteralCursor                      = CXCursor_StringLiteral
  , CharacterLiteralCursor                   = CXCursor_CharacterLiteral
  , ParenExprCursor                          = CXCursor_ParenExpr
  , UnaryOperatorCursor                      = CXCursor_UnaryOperator
  , ArraySubscriptExprCursor                 = CXCursor_ArraySubscriptExpr
  , BinaryOperatorCursor                     = CXCursor_BinaryOperator
  , CompoundAssignOperatorCursor             = CXCursor_CompoundAssignOperator
  , ConditionalOperatorCursor                = CXCursor_ConditionalOperator
  , CStyleCastExprCursor                     = CXCursor_CStyleCastExpr
  , CompoundLiteralExprCursor                = CXCursor_CompoundLiteralExpr
  , InitListExprCursor                       = CXCursor_InitListExpr
  , AddrLabelExprCursor                      = CXCursor_AddrLabelExpr
  , StmtExprCursor                           = CXCursor_StmtExpr
  , GenericSelectionExprCursor               = CXCursor_GenericSelectionExpr
  , GNUNullExprCursor                        = CXCursor_GNUNullExpr
  , CXXStaticCastExprCursor                  = CXCursor_CXXStaticCastExpr
  , CXXDynamicCastExprCursor                 = CXCursor_CXXDynamicCastExpr
  , CXXReinterpretCastExprCursor             = CXCursor_CXXReinterpretCastExpr
  , CXXConstCastExprCursor                   = CXCursor_CXXConstCastExpr
  , CXXFunctionalCastExprCursor              = CXCursor_CXXFunctionalCastExpr
  , CXXTypeidExprCursor                      = CXCursor_CXXTypeidExpr
  , CXXBoolLiteralExprCursor                 = CXCursor_CXXBoolLiteralExpr
  , CXXNullPtrLiteralExprCursor              = CXCursor_CXXNullPtrLiteralExpr
  , CXXThisExprCursor                        = CXCursor_CXXThisExpr
  , CXXThrowExprCursor                       = CXCursor_CXXThrowExpr
  , CXXNewExprCursor                         = CXCursor_CXXNewExpr
  , CXXDeleteExprCursor                      = CXCursor_CXXDeleteExpr
  , UnaryExprCursor                          = CXCursor_UnaryExpr
  , ObjCStringLiteralCursor                  = CXCursor_ObjCStringLiteral
  , ObjCEncodeExprCursor                     = CXCursor_ObjCEncodeExpr
  , ObjCSelectorExprCursor                   = CXCursor_ObjCSelectorExpr
  , ObjCProtocolExprCursor                   = CXCursor_ObjCProtocolExpr
  , ObjCBridgedCastExprCursor                = CXCursor_ObjCBridgedCastExpr
  , PackExpansionExprCursor                  = CXCursor_PackExpansionExpr
  , SizeOfPackExprCursor                     = CXCursor_SizeOfPackExpr
  , LambdaExprCursor                         = CXCursor_LambdaExpr
  , ObjCBoolLiteralExprCursor                = CXCursor_ObjCBoolLiteralExpr
  , ObjCSelfExprCursor                       = CXCursor_ObjCSelfExpr
  , UnexposedStmtCursor                      = CXCursor_UnexposedStmt
  , LabelStmtCursor                          = CXCursor_LabelStmt
  , CompoundStmtCursor                       = CXCursor_CompoundStmt
  , CaseStmtCursor                           = CXCursor_CaseStmt
  , DefaultStmtCursor                        = CXCursor_DefaultStmt
  , IfStmtCursor                             = CXCursor_IfStmt
  , SwitchStmtCursor                         = CXCursor_SwitchStmt
  , WhileStmtCursor                          = CXCursor_WhileStmt
  , DoStmtCursor                             = CXCursor_DoStmt
  , ForStmtCursor                            = CXCursor_ForStmt
  , GotoStmtCursor                           = CXCursor_GotoStmt
  , IndirectGotoStmtCursor                   = CXCursor_IndirectGotoStmt
  , ContinueStmtCursor                       = CXCursor_ContinueStmt
  , BreakStmtCursor                          = CXCursor_BreakStmt
  , ReturnStmtCursor                         = CXCursor_ReturnStmt
  , AsmStmtCursor                            = CXCursor_AsmStmt
  , ObjCAtTryStmtCursor                      = CXCursor_ObjCAtTryStmt
  , ObjCAtCatchStmtCursor                    = CXCursor_ObjCAtCatchStmt
  , ObjCAtFinallyStmtCursor                  = CXCursor_ObjCAtFinallyStmt
  , ObjCAtThrowStmtCursor                    = CXCursor_ObjCAtThrowStmt
  , ObjCAtSynchronizedStmtCursor             = CXCursor_ObjCAtSynchronizedStmt
  , ObjCAutoreleasePoolStmtCursor            = CXCursor_ObjCAutoreleasePoolStmt
  , ObjCForCollectionStmtCursor              = CXCursor_ObjCForCollectionStmt
  , CXXCatchStmtCursor                       = CXCursor_CXXCatchStmt
  , CXXTryStmtCursor                         = CXCursor_CXXTryStmt
  , CXXForRangeStmtCursor                    = CXCursor_CXXForRangeStmt
  , SEHTryStmtCursor                         = CXCursor_SEHTryStmt
  , SEHExceptStmtCursor                      = CXCursor_SEHExceptStmt
  , SEHFinallyStmtCursor                     = CXCursor_SEHFinallyStmt
  , MSAsmStmtCursor                          = CXCursor_MSAsmStmt
  , NullStmtCursor                           = CXCursor_NullStmt
  , DeclStmtCursor                           = CXCursor_DeclStmt
  , OMPParallelDirectiveCursor               = CXCursor_OMPParallelDirective
  , TranslationUnitCursor                    = CXCursor_TranslationUnit
  , UnexposedAttrCursor                      = CXCursor_UnexposedAttr
  , IBActionAttrCursor                       = CXCursor_IBActionAttr
  , IBOutletAttrCursor                       = CXCursor_IBOutletAttr
  , IBOutletCollectionAttrCursor             = CXCursor_IBOutletCollectionAttr
  , CXXFinalAttrCursor                       = CXCursor_CXXFinalAttr
  , CXXOverrideAttrCursor                    = CXCursor_CXXOverrideAttr
  , AnnotateAttrCursor                       = CXCursor_AnnotateAttr
  , AsmLabelAttrCursor                       = CXCursor_AsmLabelAttr
  , PackedAttrCursor                         = CXCursor_PackedAttr
  , PreprocessingDirectiveCursor             = CXCursor_PreprocessingDirective
  , MacroDefinitionCursor                    = CXCursor_MacroDefinition
  , MacroExpansionCursor                     = CXCursor_MacroExpansion
  , InclusionDirectiveCursor                 = CXCursor_InclusionDirective
  , ModuleImportDeclCursor                   = CXCursor_ModuleImportDecl
 };
#endc
{#enum CursorKind{} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- CursorKind ranges.
firstDeclCursor, lastDeclCursor, firstRefCursor, lastRefCursor         :: CursorKind
firstInvalidCursor, lastInvalidCursor, firstExprCursor, lastExprCursor :: CursorKind
firstStmtCursor, lastStmtCursor, firstAttrCursor, lastAttrCursor       :: CursorKind
firstPreprocessingCursor, lastPreprocessingCursor                      :: CursorKind
firstExtraDeclCursor, lastExtraDeclCursor                              :: CursorKind

firstDeclCursor          = UnexposedDeclCursor
lastDeclCursor           = CXXAccessSpecifierCursor
firstRefCursor           = ObjCSuperClassRefCursor
lastRefCursor            = VariableRefCursor
firstInvalidCursor       = InvalidFileCursor
lastInvalidCursor        = InvalidCodeCursor
firstExprCursor          = UnexposedExprCursor
lastExprCursor           = ObjCSelfExprCursor
firstStmtCursor          = UnexposedStmtCursor
lastStmtCursor           = OMPParallelDirectiveCursor
firstAttrCursor          = UnexposedAttrCursor
lastAttrCursor           = PackedAttrCursor
firstPreprocessingCursor = PreprocessingDirectiveCursor
lastPreprocessingCursor  = InclusionDirectiveCursor
firstExtraDeclCursor     = ModuleImportDeclCursor
lastExtraDeclCursor      = ModuleImportDeclCursor

-- CursorKind aliases.
gccAsmStmtCursor, macroInstantiationCursor :: CursorKind
gccAsmStmtCursor         = AsmStmtCursor
macroInstantiationCursor = MacroExpansionCursor

-- typedef struct {
--   const void *ASTNode;
--   CXTranslationUnit TranslationUnit;
-- } CXComment;
data Comment s = Comment !(Ptr ()) !(Ptr ())
                 deriving (Eq, Ord, Typeable)

instance ClangValue Comment

instance Storable (Comment s) where
    sizeOf _ = sizeOfCXComment
    {-# INLINE sizeOf #-}

    alignment _ = alignOfCXComment
    {-# INLINE alignment #-}

    peek p = do
      astNode <- {#get CXComment->ASTNode #} p
      tu <- {#get CXComment->TranslationUnit #} p
      return $! Comment astNode tu
    {-# INLINE peek #-}

    poke p (Comment astNode tu)= do
      {#set CXComment->ASTNode #} p astNode
      {#set CXComment->TranslationUnit #} p tu
    {-# INLINE poke #-}

-- typedef struct {
--   enum CXCursorKind kind;
--   int xdata;
--   const void* data[3];
-- } CXCursor;
data Cursor s = Cursor !CursorKind !Int !(Ptr ()) !(Ptr ()) !(Ptr ())
                deriving (Ord, Typeable)

instance ClangValue Cursor

instance Storable (Cursor s) where
    sizeOf _ = sizeOfCXCursor
    {-# INLINE sizeOf #-}

    alignment _ = alignOfCXCursor
    {-# INLINE alignment #-}

    peek p = do
      cursorKind <- {#get CXCursor->kind #} p
      xdata <- {#get CXCursor->xdata #} p
      cursorData <- {#get CXCursor->data #} p >>= peekArray 3
      return $! Cursor (toEnum (fromIntegral cursorKind)) (fromIntegral xdata) (cursorData !! 0) (cursorData !! 1) (cursorData !! 2)
    {-# INLINE peek #-}

    poke p (Cursor kind xdata p0 p1 p2)= do
      ptrsArray <- mallocArray 3
      pokeArray ptrsArray [p0,p1,p2]
      {#set CXCursor->kind #} p (fromIntegral (fromEnum kind))
      {#set CXCursor->xdata #} p (fromIntegral xdata)
      {#set CXCursor->data #} p (castPtr ptrsArray)
    {-# INLINE poke #-}

instance Eq (Cursor s) where
    (Cursor aK aXdata aP1 aP2 aP3) == (Cursor bK bXdata bP1 bP2 bP3) =
        (aK == bK) &&
        (aXdata == bXdata) &&
        (aP1 == bP1) &&
        (aP2' == bP2') &&
        (aP3 == bP3)
      where
        aP2' = if isDeclaration aK then intPtrToPtr 0 else aP2
        bP2' = if isDeclaration bK then intPtrToPtr 0 else bP2
    {-# INLINE (==) #-}

instance Hashable (Cursor s) where
    hashWithSalt salt (Cursor k _ p1 p2 _) =
      let p = if isExpression k || isStatement k then p2 else p1
          kindHash = hashWithSalt salt (fromEnum k)
          pAsInt = fromIntegral (ptrToIntPtr p) :: Int
      in hashWithSalt kindHash pAsInt
    {-# INLINE hash #-}

-- CXCursor clang_getNullCursor(void);
getNullCursor :: ClangBase m => ClangT s m (Cursor s)
getNullCursor =
  return $ Cursor InvalidFileCursor 0 (intPtrToPtr 0) (intPtrToPtr 0) (intPtrToPtr 0)
{-# INLINE getNullCursor #-}

-- CXCursor clang_getTranslationUnitCursor(CXTranslationUnit);
{# fun wrapped_clang_getTranslationUnitCursor as clang_getTranslationUnitCursor { id `Ptr ()' } -> `Ptr (Cursor s)' castPtr #}
getTranslationUnitCursor :: Proxy s -> TranslationUnit s' -> IO (Cursor s)
getTranslationUnitCursor _ (TranslationUnit tPtr) = clang_getTranslationUnitCursor tPtr >>= peek

-- int clang_Cursor_isNull(CXCursor);
cursor_isNull :: Cursor s -> Bool
cursor_isNull (Cursor k xdata p1 p2 p3)
  | k == InvalidFileCursor &&
    xdata == 0 &&
    p1 == intPtrToPtr 0 &&
    p2 == intPtrToPtr 0 &&
    p3 == intPtrToPtr 0
      = True
  | otherwise
      = False
{-# INLINE cursor_isNull #-}

-- unsigned clang_hashCursor(CXCursor);
{# fun clang_hashCursor {withVoided* %`Cursor a' } -> `Word32' #}
hashCursor :: Cursor s -> IO Word32
hashCursor c = clang_hashCursor c
{-# INLINEABLE hashCursor #-}

getCursorKind :: Cursor s -> CursorKind
getCursorKind (Cursor k _ _ _ _) = k
{-# INLINE getCursorKind #-}

-- unsigned clang_isDeclaration(enum CursorKind);
isDeclaration :: CursorKind -> Bool
isDeclaration k =
  (k >= firstDeclCursor && k <= lastDeclCursor) ||
  (k >= firstExtraDeclCursor && k <= lastExtraDeclCursor)
{-# INLINE isDeclaration #-}

-- unsigned clang_isReference(enum CursorKind);
isReference :: CursorKind -> Bool
isReference k =
  (k >= firstRefCursor && k <= lastRefCursor)
{-# INLINE isReference #-}

-- unsigned clang_isExpression(enum CursorKind);
isExpression :: CursorKind -> Bool
isExpression k =
  (k >= firstExprCursor && k <= lastExprCursor)
{-# INLINE isExpression #-}

-- unsigned clang_isStatement(enum CursorKind);
isStatement :: CursorKind -> Bool
isStatement k =
  (k >= firstStmtCursor && k <= lastStmtCursor)
{-# INLINE isStatement #-}

isAttribute :: CursorKind -> Bool
isAttribute k =
  (k >= firstAttrCursor && k <= lastAttrCursor)
{-# INLINE isAttribute #-}

-- unsigned clang_isInvalid(enum CursorKind);
isInvalid :: CursorKind -> Bool
isInvalid k =
  (k >= firstInvalidCursor && k <= lastInvalidCursor)
{-# INLINE isInvalid #-}

-- unsigned clang_isTranslationUnit(enum CursorKind);
isTranslationUnit :: CursorKind -> Bool
isTranslationUnit TranslationUnitCursor = True
isTranslationUnit _ = False
{-# INLINE isTranslationUnit #-}

-- unsigned clang_isPreprocessing(enum CursorKind);
isPreprocessing :: CursorKind -> Bool
isPreprocessing k =
  (k >= firstPreprocessingCursor && k <= lastPreprocessingCursor)
{-# INLINE isPreprocessing #-}

-- unsigned clang_isUnexposed(enum CursorKind);
isUnexposed :: CursorKind -> Bool
isUnexposed k =
  (k >= firstPreprocessingCursor && k <= lastPreprocessingCursor)
{-# INLINE isUnexposed #-}

-- enum CXLinkageKind {
--   CXLinkage_Invalid,
--   CXLinkage_NoLinkage,
--   CXLinkage_Internal,
--   CXLinkage_UniqueExternal,
--   CXLinkage_External
-- };
#c
enum LinkageKind
  { Linkage_Invalid        = CXLinkage_Invalid
  , Linkage_NoLinkage      = CXLinkage_NoLinkage
  , Linkage_Internal       = CXLinkage_Internal
  , Linkage_UniqueExternal = CXLinkage_UniqueExternal
  , Linkage_External       = CXLinkage_External
 };
#endc
{#enum LinkageKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CXLinkageKind clang_getCursorLinkage(CXCursor cursor);
{# fun clang_getCursorLinkage {withVoided* %`Cursor a' } -> `Int' #}
getCursorLinkage :: Cursor s -> IO LinkageKind
getCursorLinkage c = clang_getCursorLinkage c >>= return . toEnum

-- enum CXAvailabilityKind clang_getCursorAvailability(CXCursor cursor);
{# fun clang_getCursorAvailability {withVoided* %`Cursor a' } -> `Int' #}
getCursorAvailability :: Cursor s -> IO AvailabilityKind
getCursorAvailability c = clang_getCursorAvailability c >>= return . toEnum

data PlatformAvailability = PlatformAvailability
  { availabilityPlatform      :: !B.ByteString
  , availabilityIntroduced    :: !Version
  , availabilityDeprecated    :: !Version
  , availabilityObsoleted     :: !Version
  , availabilityIsUnavailable :: !Bool
  , availabilityMessage       :: !B.ByteString
  } deriving (Eq, Ord, Typeable)

instance Storable PlatformAvailability where
  sizeOf _ = sizeOfCXPlatformAvailability
  {-# INLINE sizeOf #-}

  alignment _ = alignOfCXPlatformAvailability
  {-# INLINE alignment #-}

  peek p = do
    (ClangString platD platF) <- peekByteOff p offsetCXPlatformAvailabilityPlatform
    platform <- B.packCString =<< getCStringPtr platD platF

    introduced <- peekByteOff p offsetCXPlatformAvailabilityIntroduced
    deprecated <- peekByteOff p offsetCXPlatformAvailabilityDeprecated
    obsoleted <- peekByteOff p offsetCXPlatformAvailabilityObsoleted
    intUnavailable :: Int <- fromCInt <$> peekByteOff p offsetCXPlatformAvailabilityUnavailable

    (ClangString msgD msgF) <- peekByteOff p offsetCXPlatformAvailabilityMessage
    message <- B.packCString =<< getCStringPtr msgD msgF

    return $! PlatformAvailability platform introduced deprecated obsoleted
                                   (if intUnavailable == 0 then False else True)
                                   message
  {-# INLINE peek #-}

  -- Since we can't construct ClangStrings, we can't really poke these.
  poke _ _ = undefined
  {-# INLINE poke #-}

-- void clang_disposeCXPlatformAvailability(CXPlatformAvailability);
{# fun clang_disposeCXPlatformAvailability { id `Ptr ()' } -> `()' #}
disposeCXPlatformAvailability :: Ptr PlatformAvailability -> IO ()
disposeCXPlatformAvailability p = clang_disposeCXPlatformAvailability (castPtr p)

-- int clang_getCursorPlatformAvailability(CXCursor cursor,
--                                         int *always_deprecated,
--                                         CXString *deprecated_message,
--                                         int *always_unavailable,
--                                         CXString *unavailable_message,
--                                         CXPlatformAvailability *availability,
--                                         int availability_size);
{# fun clang_getCursorPlatformAvailability {withVoided* %`Cursor a', alloca- `CInt' peek*, id `Ptr ()', alloca- `CInt' peek*, id `Ptr ()', id `Ptr ()', `Int' } -> `Int' #}
unsafe_getCursorPlatformAvailability :: Cursor s' -> Ptr PlatformAvailability -> Int -> IO (Bool, ClangString (), Bool, ClangString (), Int)
unsafe_getCursorPlatformAvailability c dest destLen =
 alloca (\(dMsgPtr :: (Ptr (ClangString ()))) ->
 alloca (\(uMsgPtr :: (Ptr (ClangString ()))) -> do
   (size, ad, au) <- clang_getCursorPlatformAvailability
                       c
                       (castPtr dMsgPtr)
                       (castPtr uMsgPtr)
                       (castPtr dest)
                       destLen
   dm <- peek dMsgPtr
   um <- peek uMsgPtr
   return (toBool ad, dm, toBool au, um, size)))

data PlatformAvailabilityInfo s = PlatformAvailabilityInfo
  { availabilityAlwaysDeprecated   :: !Bool
  , availabilityDeprecatedMessage  :: !(ClangString s)
  , availabilityAlwaysUnavailable  :: !Bool
  , availabilityUnavailableMessage :: !(ClangString s)
  , availabilityInfo               :: ![PlatformAvailability]
  } deriving (Eq, Ord, Typeable)

instance ClangValue PlatformAvailabilityInfo

getCursorPlatformAvailability :: ClangBase m => Cursor s'
                              -> ClangT s m (PlatformAvailabilityInfo s)
getCursorPlatformAvailability c = do
    (ad, dMsg, au, uMsg, infos) <-
      liftIO $ allocaArray maxPlatformAvailability $ \p -> do
        (ad, dMsg, au, uMsg, outSize) <-
          unsafe_getCursorPlatformAvailability c p maxPlatformAvailability
        let count = min outSize maxPlatformAvailability
        infos <- peekArray count p
        forM_ [0..(count - 1)] $ \idx -> do
          disposeCXPlatformAvailability (advancePtr p idx)
        return (ad, dMsg, au, uMsg, infos)
    dMsg' <- registerClangString $ return dMsg
    uMsg' <- registerClangString $ return uMsg
    return $ PlatformAvailabilityInfo ad dMsg' au uMsg' infos
  where
    maxPlatformAvailability = 32

-- enum CXLanguageKind {
--   CXLanguage_Invalid = 0,
--   CXLanguage_C,
--   CXLanguage_ObjC,
--   CXLanguage_CPlusPlus
-- };
#c
enum LanguageKind {
    Language_Invalid   = CXLanguage_Invalid
  , Language_C         = CXLanguage_C
  , Language_ObjC      = CXLanguage_ObjC
  , Language_CPlusPlus = CXLanguage_CPlusPlus
  };
#endc

{#enum LanguageKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CXLanguageKind clang_getCursorLanguage(CXCursor cursor);
{# fun clang_getCursorLanguage {withVoided* %`Cursor a' } -> `Int' #}
getCursorLanguage :: Cursor s -> IO LanguageKind
getCursorLanguage c = clang_getCursorLanguage c >>= return . toEnum

-- CXTranslationUnit clang_Cursor_getTranslationUnit(CXCursor);
{# fun clang_Cursor_getTranslationUnit {withVoided* %`Cursor a' } -> `Ptr ()' id #}
unsafe_Cursor_getTranslationUnit :: Cursor s -> IO (TranslationUnit ())
unsafe_Cursor_getTranslationUnit c =
  clang_Cursor_getTranslationUnit c >>= return . mkTranslationUnit

-- Note that we do not register the translation unit! This function
-- never creates a 'new' translation unit, so we don't need to dispose
-- it. Moreover, since translation units aren't reference counted doing
-- so will lead to crashes.
cursor_getTranslationUnit :: ClangBase m => Cursor s' -> ClangT s m (TranslationUnit s)
cursor_getTranslationUnit c = liftIO $ unsafeCoerce <$> unsafe_Cursor_getTranslationUnit c

-- typedef struct CXCursorSetImpl *CXCursorSet;
newtype CursorSet s = CursorSet { unCursorSet :: Ptr () }
                      deriving (Eq, Ord, Typeable)

instance ClangValue CursorSet

-- void clang_disposeCXCursorSet(CXCursorSet cset);
{# fun clang_disposeCXCursorSet { id `Ptr ()' } -> `()' #}
disposeCXCursorSet :: CursorSet s -> IO ()
disposeCXCursorSet cs = clang_disposeCXCursorSet (unCursorSet cs)

registerCursorSet :: ClangBase m => IO (CursorSet ()) -> ClangT s m (CursorSet s)
registerCursorSet action = do
  (_, idx) <- clangAllocate (action >>= return . unsafeCoerce)
                            (\i -> disposeCXCursorSet i)
  return idx
{-# INLINEABLE registerCursorSet #-}

-- CXCursorSet clang_createCXCursorSet();
{# fun clang_createCXCursorSet as clang_createCXCursorSet {} -> `Ptr ()' id #}
unsafe_createCXCursorSet :: IO (CursorSet ())
unsafe_createCXCursorSet = clang_createCXCursorSet >>= return . CursorSet

createCXCursorSet :: ClangBase m => ClangT s m (CursorSet s)
createCXCursorSet = registerCursorSet unsafe_createCXCursorSet

-- unsigned clang_CXCursorSet_contains(CXCursorSet cset, CXCursor cursor);
{# fun clang_CXCursorSet_contains {id `Ptr ()' ,withVoided* %`Cursor a' } -> `Bool' toBool #}
cXCursorSet_contains :: CursorSet s -> Cursor s' -> IO Bool
cXCursorSet_contains cs c =
  clang_CXCursorSet_contains (unCursorSet cs) c

-- unsigned clang_CXCursorSet_insert(CXCursorSet cset, CXCursor cursor);
{# fun clang_CXCursorSet_insert {id `Ptr ()', withVoided* %`Cursor a' } -> `Bool' toBool #}
cXCursorSet_insert :: CursorSet s -> Cursor s' -> IO Bool
cXCursorSet_insert cs c =
  clang_CXCursorSet_insert (unCursorSet cs) c

-- CXCursor clang_getCursorSemanticParent(CXCursor cursor);
{# fun wrapped_clang_getCursorSemanticParent as clang_getCursorSemanticParent {withVoided* %`Cursor a' } -> `Ptr (Cursor s)' castPtr #}
getCursorSemanticParent :: Proxy s -> Cursor s' -> IO (Cursor s)
getCursorSemanticParent _ c =
  clang_getCursorSemanticParent c >>= peek

-- CXCursor clang_getCursorLexicalParent(CXCursor cursor);
{# fun wrapped_clang_getCursorLexicalParent as clang_getCursorLexicalParent {withVoided* %`Cursor a' } -> `Ptr (Cursor s)' castPtr #}
getCursorLexicalParent :: Proxy s -> Cursor s' -> IO (Cursor s)
getCursorLexicalParent _ c =
  clang_getCursorLexicalParent c >>= peek

-- void clang_disposeOverriddenCursors(CXCursor *overridden);
{# fun clang_disposeOverriddenCursors { id `Ptr ()' } -> `()' #}
disposeOverridden :: CursorList s -> IO ()
disposeOverridden cl =
 let (csPtr, _) = fromCursorList cl in
 clang_disposeOverriddenCursors csPtr

registerOverriddenList :: ClangBase m => IO UnsafeCursorList -> ClangT s m (CursorList s)
registerOverriddenList action = do
    (_, cursorList) <- clangAllocate (action >>= cursorListToVector) disposeOverridden
    return cursorList
{-# INLINEABLE registerOverriddenList #-}

-- void clang_getOverriddenCursors(CXCursor cursor, CXCursor **overridden, unsigned *num_overridden);
{# fun clang_getOverriddenCursors {withVoided* %`Cursor a', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getOverriddenCursors :: Cursor s -> IO UnsafeCursorList
unsafe_getOverriddenCursors c = do
  cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr (Cursor s)))))
  n <- clang_getOverriddenCursors c (castPtr cxListPtr)
  free cxListPtr
  return (toCursorList (cxListPtr, (fromIntegral n)))

getOverriddenCursors :: ClangBase m => Cursor s' -> ClangT s m (CursorList s)
getOverriddenCursors = registerOverriddenList . unsafe_getOverriddenCursors

-- CXFile clang_getIncludedFile(CXCursor cursor);
{# fun clang_getIncludedFile {withVoided* %`Cursor a' } -> `Ptr ()' id #}
getIncludedFile :: Proxy s -> Cursor s' -> IO (Maybe (File s))
getIncludedFile _ c =
  clang_getIncludedFile c >>= return . maybeFile . File . castPtr

-- CXCursor clang_getCursor(CXTranslationUnit, CXSourceLocation);
{# fun wrapped_clang_getCursor as clang_getCursor { id `Ptr ()' , withVoided* `SourceLocation a' } -> `Ptr (Cursor s)' castPtr #}
getCursor :: Proxy s -> TranslationUnit s' -> SourceLocation s'' -> IO (Cursor s)
getCursor _ t s = clang_getCursor (unTranslationUnit t) s >>= peek

-- CXSourceLocation clang_getCursorLocation(CXCursor);
{# fun wrapped_clang_getCursorLocation as clang_getCursorLocation {withVoided* %`Cursor a' } -> `Ptr (SourceLocation s)' castPtr #}
getCursorLocation :: Proxy s -> Cursor s' -> IO (SourceLocation s)
getCursorLocation _ c =
  clang_getCursorLocation c>>= peek
{-# INLINEABLE getCursorLocation #-}

-- Variant of clang_getCursorLocation that fuses a call to clang_getSpellingLocation.
getCursorSpellingLocation :: Proxy s -> Cursor s' -> IO (Maybe (File s), Int, Int, Int)
getCursorSpellingLocation _ c =
  allocaBytes {#sizeof PtrPtrCXFile #} (\ptrToFilePtr ->
  alloca (\(linePtr :: (Ptr CUInt)) ->
  alloca (\(columnPtr :: (Ptr CUInt)) ->
  alloca (\(offsetPtr :: (Ptr CUInt)) -> do
    slPtr <- clang_getCursorLocation c
    peek slPtr >>= \sl -> clang_getSpellingLocation sl ptrToFilePtr linePtr columnPtr offsetPtr
    filePtr <- {#get *CXFile #} ptrToFilePtr
    let _maybeFile = maybeFile (File filePtr)
    line <- peek linePtr
    column <- peek columnPtr
    offset <- peek offsetPtr
    return (_maybeFile, fromIntegral line, fromIntegral column, fromIntegral offset)))))
{-# INLINEABLE getCursorSpellingLocation #-}

-- CXSourceRange clang_getCursorExtent(CXCursor);
{# fun wrapped_clang_getCursorExtent as clang_getCursorExtent {withVoided* %`Cursor a' } -> `Ptr (SourceRange s)' castPtr #}
getCursorExtent :: Proxy s -> Cursor s' -> IO (SourceRange s)
getCursorExtent _ c =
  clang_getCursorExtent c >>= peek
{-# INLINEABLE getCursorExtent #-}

-- enum CXTypeKind {
--   CXType_Invalid = 0,
--   CXType_Unexposed = 1,
--   CXType_Void = 2,
--   CXType_Bool = 3,
--   CXType_Char_U = 4,
--   CXType_UChar = 5,
--   CXType_Char16 = 6,
--   CXType_Char32 = 7,
--   CXType_UShort = 8,
--   CXType_UInt = 9,
--   CXType_ULong = 10,
--   CXType_ULongLong = 11,
--   CXType_UInt128 = 12,
--   CXType_Char_S = 13,
--   CXType_SChar = 14,
--   CXType_WChar = 15,
--   CXType_Short = 16,
--   CXType_Int = 17,
--   CXType_Long = 18,
--   CXType_LongLong = 19,
--   CXType_Int128 = 20,
--   CXType_Float = 21,
--   CXType_Double = 22,
--   CXType_LongDouble = 23,
--   CXType_NullPtr = 24,
--   CXType_Overload = 25,
--   CXType_Dependent = 26,
--   CXType_ObjCId = 27,
--   CXType_ObjCClass = 28,
--   CXType_ObjCSel = 29,
--   CXType_FirstBuiltin = CXType_Void,
--   CXType_LastBuiltin  = CXType_ObjCSel,
--   CXType_Complex = 100,
--   CXType_Pointer = 101,
--   CXType_BlockPointer = 102,
--   CXType_LValueReference = 103,
--   CXType_RValueReference = 104,
--   CXType_Record = 105,
--   CXType_Enum = 106,
--   CXType_Typedef = 107,
--   CXType_ObjCInterface = 108,
--   CXType_ObjCObjectPointer = 109,
--   CXType_FunctionNoProto = 110,
--   CXType_FunctionProto = 111
--   CXType_ConstantArray = 112
--   CXType_Vector = 113,
--   CXType_IncompleteArray = 114,
--   CXType_VariableArray = 115,
--   CXType_DependentSizedArray = 116,
--   CXType_MemberPointer = 117
-- };

#c
enum TypeKind
  { Type_Invalid             = CXType_Invalid
  , Type_Unexposed           = CXType_Unexposed
  , Type_Void                = CXType_Void
  , Type_Bool                = CXType_Bool
  , Type_Char_U              = CXType_Char_U
  , Type_UChar               = CXType_UChar
  , Type_Char16              = CXType_Char16
  , Type_Char32              = CXType_Char32
  , Type_UShort              = CXType_UShort
  , Type_UInt                = CXType_UInt
  , Type_ULong               = CXType_ULong
  , Type_ULongLong           = CXType_ULongLong
  , Type_UInt128             = CXType_UInt128
  , Type_Char_S              = CXType_Char_S
  , Type_SChar               = CXType_SChar
  , Type_WChar               = CXType_WChar
  , Type_Short               = CXType_Short
  , Type_Int                 = CXType_Int
  , Type_Long                = CXType_Long
  , Type_LongLong            = CXType_LongLong
  , Type_Int128              = CXType_Int128
  , Type_Float               = CXType_Float
  , Type_Double              = CXType_Double
  , Type_LongDouble          = CXType_LongDouble
  , Type_NullPtr             = CXType_NullPtr
  , Type_Overload            = CXType_Overload
  , Type_Dependent           = CXType_Dependent
  , Type_ObjCId              = CXType_ObjCId
  , Type_ObjCClass           = CXType_ObjCClass
  , Type_ObjCSel             = CXType_ObjCSel
  , Type_Complex             = CXType_Complex
  , Type_Pointer             = CXType_Pointer
  , Type_BlockPointer        = CXType_BlockPointer
  , Type_LValueReference     = CXType_LValueReference
  , Type_RValueReference     = CXType_RValueReference
  , Type_Record              = CXType_Record
  , Type_Enum                = CXType_Enum
  , Type_Typedef             = CXType_Typedef
  , Type_ObjCInterface       = CXType_ObjCInterface
  , Type_ObjCObjectPointer   = CXType_ObjCObjectPointer
  , Type_FunctionNoProto     = CXType_FunctionNoProto
  , Type_FunctionProto       = CXType_FunctionProto
  , Type_ConstantArray       = CXType_ConstantArray
  , Type_Vector              = CXType_Vector
  , Type_IncompleteArray     = CXType_IncompleteArray
  , Type_VariableArray       = CXType_VariableArray
  , Type_DependentSizedArray = CXType_DependentSizedArray
  , Type_MemberPointer       = CXType_MemberPointer
 };
#endc
{# enum TypeKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

type_FirstBuiltin, type_LastBuiltin :: TypeKind
type_FirstBuiltin = Type_Void
type_LastBuiltin  = Type_ObjCSel

-- enum CXCallingConv {
--   CXCallingConv_Default = 0,
--   CXCallingConv_C = 1,
--   CXCallingConv_X86StdCall = 2,
--   CXCallingConv_X86FastCall = 3,
--   CXCallingConv_X86ThisCall = 4,
--   CXCallingConv_X86Pascal = 5,
--   CXCallingConv_AAPCS = 6,
--   CXCallingConv_AAPCS_VFP = 7,
--   CXCallingConv_IntelOclBicc = 9,
--   CXCallingConv_X86_64Win64 = 10,
--   CXCallingConv_X86_64SysV = 11,
--   CXCallingConv_Invalid = 100,
--   CXCallingConv_Unexposed = 200
-- };
#c
enum CallingConv
  { CallingConv_Default     = CXCallingConv_Default
  , CallingConv_C           = CXCallingConv_C
  , CallingConv_X86StdCall  = CXCallingConv_X86StdCall
  , CallingConv_X86FastCall = CXCallingConv_X86FastCall
  , CallingConv_X86ThisCall = CXCallingConv_X86ThisCall
  , CallingConv_X86Pascal   = CXCallingConv_X86Pascal
  , CallingConv_AAPCS       = CXCallingConv_AAPCS
  , CallingConv_AAPCS_VFP   = CXCallingConv_AAPCS_VFP
  , CallingConv_IntelOclBic = CXCallingConv_IntelOclBicc
  , CallingConv_X86_64Win64 = CXCallingConv_X86_64Win64
  , CallingConv_X86_64SysV  = CXCallingConv_X86_64SysV
  , CallingConv_Invalid     = CXCallingConv_Invalid
  , CallingConv_Unexposed   = CXCallingConv_Unexposed
 };
#endc
{# enum CallingConv {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- typedef struct {
--   enum CXTypeKind kind;
--   void *data[2];
-- } CXType;
data Type s = Type !TypeKind !(Ptr ()) !(Ptr ())
              deriving (Eq, Ord, Typeable)

instance ClangValue Type

instance Storable (Type s) where
    sizeOf _ = {#sizeof CXType #}
    {-# INLINE sizeOf #-}

    alignment _ = {#alignof CXType #}
    {-# INLINE alignment #-}

    peek p = do
      kindCInt <- {#get CXType->kind #} p
      ptrsArray <- {#get CXType->data #} p >>= peekArray 2
      return $! Type (toEnum (fromIntegral kindCInt)) (ptrsArray !! 0) (ptrsArray !! 1)
    {-# INLINE peek #-}

    poke p (Type kind p0 p1)= do
      ptrsArray <- mallocArray 2
      pokeArray ptrsArray [p0,p1]
      {#set CXType->kind #} p (fromIntegral (fromEnum kind))
      {#set CXType->data #} p (castPtr ptrsArray)
    {-# INLINE poke #-}

getTypeKind :: Type s -> TypeKind
getTypeKind (Type k _ _) = k

-- CXType clang_getCursorType(CXCursor C);
{# fun wrapped_clang_getCursorType as clang_getCursorType {withVoided* %`Cursor a' } -> `Ptr (Type s)' castPtr #}
getCursorType :: Proxy s -> Cursor s' -> IO (Type s)
getCursorType proxy c =
  clang_getCursorType c >>= peek

-- CXString clang_getTypeSpelling(CXType CT);
{# fun wrapped_clang_getTypeSpelling as clang_getTypeSpelling {withVoided* %`Type a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getTypeSpelling :: (Type s) -> IO (ClangString ())
unsafe_getTypeSpelling t = do
 cxString <- clang_getTypeSpelling t
 peek cxString

getTypeSpelling :: ClangBase m => (Type s') -> ClangT s m (ClangString s)
getTypeSpelling = registerClangString . unsafe_getTypeSpelling

-- CXType clang_getTypedefDeclUnderlyingType(CXCursor C);
{# fun wrapped_clang_getTypedefDeclUnderlyingType as clang_getTypedefDeclUnderlyingType {withVoided* %`Cursor a' } -> `Ptr (Type s)' castPtr #}
getTypedefDeclUnderlyingType :: Proxy s -> Cursor s' -> IO (Type s)
getTypedefDeclUnderlyingType proxy c =
  clang_getTypedefDeclUnderlyingType c >>= peek

-- CXType clang_getEnumDeclIntegerType(CXCursor C);
{# fun wrapped_clang_getEnumDeclIntegerType as clang_getEnumDeclIntegerType {withVoided* %`Cursor a' } -> `Ptr (Type s)' castPtr #}
getEnumDeclIntegerType :: Proxy s -> Cursor s' -> IO (Type s)
getEnumDeclIntegerType proxy c =
  clang_getEnumDeclIntegerType c >>= peek

-- long long clang_getEnumConstantDeclValue(CXCursor);
{# fun clang_getEnumConstantDeclValue {withVoided* %`Cursor a' } -> `Int64' #}
getEnumConstantDeclValue :: Cursor s -> IO Int64
getEnumConstantDeclValue c = clang_getEnumConstantDeclValue c

-- unsigned long long clang_getEnumConstantDeclUnsignedValue(CXCursor);
{# fun clang_getEnumConstantDeclUnsignedValue {withVoided* %`Cursor a' } -> `Word64' #}
getEnumConstantDeclUnsignedValue :: Cursor s -> IO Word64
getEnumConstantDeclUnsignedValue c = clang_getEnumConstantDeclUnsignedValue c

-- int clang_getFieldDeclBitWidth(CXCursor C);
-- %fun clang_getFieldDeclBitWidth :: Cursor s -> IO Int
{# fun clang_getFieldDeclBitWidth {withVoided* %`Cursor a' } -> `Int' #}
getFieldDeclBitWidth :: Cursor s -> IO Int
getFieldDeclBitWidth c = clang_getFieldDeclBitWidth c

-- int clang_Cursor_getNumArguments(CXCursor C);
{# fun clang_Cursor_getNumArguments {withVoided* %`Cursor a' } -> `Int' #}
cursor_getNumArguments :: Cursor s -> IO Int
cursor_getNumArguments c = clang_Cursor_getNumArguments c

-- CXCursor clang_Cursor_getArgument(CXCursor C, unsigned i);
{# fun wrapped_clang_Cursor_getArgument as clang_Cursor_getArgument {withVoided* %`Cursor a', `Int' } -> `Ptr (Cursor s)' castPtr #}
cursor_getArgument :: Proxy s -> Cursor s' -> Int -> IO (Cursor s)
cursor_getArgument _ c i = clang_Cursor_getArgument c i >>= peek

-- unsigned clang_equalTypes(CXType A, CXType B);
{# fun clang_equalTypes {withVoided* %`Type a',withVoided* %`Type b' } -> `Bool' toBool #}
equalTypes :: Type s -> Type s' -> IO Bool
equalTypes t1 t2 = clang_equalTypes t1 t2

-- CXType clang_getCanonicalType(CXType T);
{# fun wrapped_clang_getCanonicalType as clang_getCanonicalType {withVoided* %`Type a' } -> `Ptr (Type s)' castPtr #}
getCanonicalType :: Proxy s -> Type s' -> IO (Type s)
getCanonicalType proxy t = do
  cPtr <- clang_getCanonicalType t
  peek cPtr

-- unsigned clang_isConstQualifiedType(CXType T);
{# fun clang_isConstQualifiedType {withVoided* %`Type a' } -> `Bool' toBool #}
isConstQualifiedType :: Type s -> IO Bool
isConstQualifiedType t = clang_isConstQualifiedType t

-- unsigned clang_isVolatileQualifiedType(CXType T);
{# fun clang_isVolatileQualifiedType {withVoided* %`Type a' } -> `Bool' toBool #}
isVolatileQualifiedType :: Type s -> IO Bool
isVolatileQualifiedType t = clang_isVolatileQualifiedType t

-- unsigned clang_isRestrictQualifiedType(CXType T);
{# fun clang_isRestrictQualifiedType {withVoided* %`Type a' } -> `Bool' toBool #}
isRestrictQualifiedType :: Type s -> IO Bool
isRestrictQualifiedType t = clang_isRestrictQualifiedType t

-- CXType clang_getPointeeType(CXType T);
{# fun wrapped_clang_getPointeeType as clang_getPointeeType { withVoided* %`Type a' } -> `Ptr (Type s)' castPtr #}
getPointeeType :: Proxy s -> Type s' -> IO (Type s)
getPointeeType proxy t = do
  cPtr <- clang_getPointeeType t
  peek cPtr

-- CXCursor clang_getTypeDeclaration(CXType T);
{# fun wrapped_clang_getTypeDeclaration as clang_getTypeDeclaration { withVoided* %`Type a' } -> `Ptr (Cursor s)' castPtr #}
getTypeDeclaration :: Proxy s -> Type s' -> IO (Cursor s)
getTypeDeclaration _ t = do
  cPtr <- clang_getTypeDeclaration t
  peek cPtr

-- CXString clang_getDeclObjCTypeEncoding(CXCursor C);
{# fun wrapped_clang_getDeclObjCTypeEncoding as clang_getDeclObjCTypeEncoding {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getDeclObjCTypeEncoding :: Cursor s -> IO (ClangString ())
unsafe_getDeclObjCTypeEncoding c = clang_getDeclObjCTypeEncoding c >>= peek

getDeclObjCTypeEncoding :: ClangBase m => Cursor s' -> ClangT s m (ClangString s)
getDeclObjCTypeEncoding = registerClangString . unsafe_getDeclObjCTypeEncoding

-- CXString clang_getTypeKindSpelling(enum CXTypeKind K);
{# fun wrapped_clang_getTypeKindSpelling as clang_getTypeKindSpelling { `CInt' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getTypeKindSpelling :: TypeKind -> IO (ClangString ())
unsafe_getTypeKindSpelling tk = clang_getTypeKindSpelling (fromIntegral (fromEnum tk)) >>= peek

getTypeKindSpelling :: ClangBase m => TypeKind -> ClangT s m (ClangString s)
getTypeKindSpelling = registerClangString . unsafe_getTypeKindSpelling

-- enum CXCallingConv clang_getFunctionTypeCallingConv(CXType T);
{# fun clang_getFunctionTypeCallingConv { withVoided* %`Type a' } -> `CInt' id #}
getFunctionTypeCallingConv :: Type s' -> IO CallingConv
getFunctionTypeCallingConv t = clang_getFunctionTypeCallingConv t >>= return . toEnum . fromIntegral

-- CXType clang_getResultType(CXType T);
{# fun wrapped_clang_getResultType as clang_getResultType { withVoided* %`Type a' } -> `Ptr (Type s)' castPtr #}
getResultType :: Proxy s -> Type s' -> IO (Type s)
getResultType proxy t = clang_getResultType t >>= peek

-- CXType clang_getNumArgTypes(CXType T);
{# fun clang_getNumArgTypes { withVoided* %`Type a' } -> `Int' #}
getNumArgTypes :: Type s -> IO Int
getNumArgTypes t = clang_getNumArgTypes t

-- CXType clang_getArgType(CXType T, int i);
{# fun wrapped_clang_getArgType as clang_getArgType { withVoided* %`Type a', `Int' } -> `Ptr (Type s)' castPtr #}
getArgType :: Proxy s -> Type s' -> Int -> IO (Type s)
getArgType proxy t i = clang_getArgType t i >>= peek

-- unsigned clang_isFunctionTypeVariadic(CXType T);
{# fun clang_isFunctionTypeVariadic { withVoided* %`Type a' } -> `CUInt' id #}
isFunctionTypeVariadic :: Type s -> IO Bool
isFunctionTypeVariadic t = clang_isFunctionTypeVariadic t >>= return . (toBool :: Int -> Bool) . fromIntegral

-- CXType clang_getCursorResultType(CXCursor C);
{# fun wrapped_clang_getCursorResultType as clang_getCursorResultType {withVoided* %`Cursor a' } -> `Ptr (Type s)' castPtr #}
getCursorResultType :: Proxy s -> Cursor s' -> IO (Type s)
getCursorResultType p c = clang_getCursorResultType c >>= peek

-- unsigned clang_isPODType(CXType T);
{# fun clang_isPODType {withVoided* %`Type a' } -> `Bool' toBool #}
isPODType :: Type s -> IO Bool
isPODType t = clang_isPODType t

-- CXType clang_getElementType(CXType T);
{# fun wrapped_clang_getElementType as clang_getElementType { withVoided* %`Type a' } -> `Ptr (Type s)' castPtr #}
getElementType :: Proxy s -> Type s' -> IO (Type s)
getElementType proxy t = clang_getElementType t >>= peek

-- long long clang_getNumElements(CXType T);
{# fun clang_getNumElements { withVoided* %`Type a' } -> `Int64' #}
getNumElements :: Type s -> IO Int64
getNumElements t = clang_getNumElements t

-- CXType clang_getArrayElementType(CXType T);
{# fun wrapped_clang_getArrayElementType as clang_getArrayElementType { withVoided* %`Type a' } -> `Ptr (Type s)' castPtr #}
getArrayElementType :: Proxy s -> Type s' -> IO (Type s)
getArrayElementType proxy t = clang_getArrayElementType t >>= peek

-- long long clang_getArraySize(CXType T);
{# fun clang_getArraySize { withVoided* %`Type a' } -> `Int64' #}
getArraySize :: Type s -> IO Int64
getArraySize t = clang_getArraySize t

-- enum CXTypeLayoutError {
--   CXTypeLayoutError_Invalid = -1,
--   CXTypeLayoutError_Incomplete = -2,
--   CXTypeLayoutError_Dependent = -3,
--   CXTypeLayoutError_NotConstantSize = -4,
--   CXTypeLayoutError_InvalidFieldName = -5
-- };
#c
enum TypeLayoutError
  { TypeLayoutError_Invalid          = CXTypeLayoutError_Invalid
  , TypeLayoutError_Incomplete       = CXTypeLayoutError_Incomplete
  , TypeLayoutError_Dependent        = CXTypeLayoutError_Dependent
  , TypeLayoutError_NotConstantSize  = CXTypeLayoutError_NotConstantSize
  , TypeLayoutError_InvalidFieldName = CXTypeLayoutError_InvalidFieldName
 };
#endc
{# enum TypeLayoutError {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

int64OrLayoutError :: Int64 -> Either TypeLayoutError Int64
int64OrLayoutError v | v == -1   = Left TypeLayoutError_Invalid
                     | v == -2   = Left TypeLayoutError_Incomplete
                     | v == -3   = Left TypeLayoutError_Dependent
                     | v == -4   = Left TypeLayoutError_NotConstantSize
                     | v == -5   = Left TypeLayoutError_InvalidFieldName
                     | otherwise = Right v

-- long long clang_Type_getAlignOf(CXType T);
{# fun clang_Type_getAlignOf { withVoided* %`Type a' } -> `Int64' #}
type_getAlignOf :: Type s -> IO (Either TypeLayoutError Int64)
type_getAlignOf t = clang_Type_getAlignOf t >>= return . int64OrLayoutError

-- CXType clang_Type_getClassType(CXType T);
{# fun wrapped_clang_Type_getClassType as clang_Type_getClassType { withVoided* %`Type a' } -> `Ptr (Type s)' castPtr #}
type_getClassType :: Proxy s -> Type s' -> IO (Type s)
type_getClassType proxy t = clang_getElementType t >>= peek

-- long long clang_Type_getSizeOf(CXType T);
{# fun clang_Type_getSizeOf { withVoided* %`Type a' } -> `Int64' #}
type_getSizeOf :: Type s -> IO (Either TypeLayoutError Int64)
type_getSizeOf t = clang_Type_getSizeOf t >>= return . int64OrLayoutError

-- long long clang_Type_getOffsetOf(CXType T, const char *S);
{# fun clang_Type_getOffsetOf { withVoided* %`Type a' , `CString' } -> `Int64' #}
unsafe_Type_getOffsetOf :: Type s -> CString -> IO (Either TypeLayoutError Int64)
unsafe_Type_getOffsetOf t s = clang_Type_getOffsetOf t s >>= return . int64OrLayoutError

type_getOffsetOf :: ClangBase m => Type s' -> B.ByteString
                 -> ClangT s m (Either TypeLayoutError Int64)
type_getOffsetOf t field = liftIO $ B.useAsCString field $ \cField ->
  unsafe_Type_getOffsetOf t cField

-- enum CXRefQualifierKind {
--   CXRefQualifier_None = 0,
--   CXRefQualifier_LValue,
--   CXRefQualifier_RValue
-- };
#c
enum RefQualifierKind
  { RefQualifier_None   = CXRefQualifier_None
  , RefQualifier_LValue = CXRefQualifier_LValue
  , RefQualifier_RValue = CXRefQualifier_RValue
  };
#endc
{# enum RefQualifierKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CXRefQualifierKind clang_Type_getCXXRefQualifier(CXType T);
{# fun clang_Type_getCXXRefQualifier { withVoided* %`Type a' } -> `Int' #}
type_getCXXRefQualifier :: Type s -> IO RefQualifierKind
type_getCXXRefQualifier t = clang_Type_getCXXRefQualifier t >>= return . toEnum


-- unsigned clang_Cursor_isBitField(CXCursor C);
{# fun clang_Cursor_isBitField {withVoided* %`Cursor a' } -> `Bool' toBool #}
isBitField :: Cursor s -> IO Bool
isBitField c = clang_Cursor_isBitField c

-- unsigned clang_isVirtualBase(CXCursor);
{# fun clang_isVirtualBase {withVoided* %`Cursor a' } -> `Bool' toBool #}
isVirtualBase :: Cursor s -> IO Bool
isVirtualBase c = clang_Cursor_isBitField c

-- enum CX_CXXAccessSpecifier {
--   CX_CXXInvalidAccessSpecifier,
--   CX_CXXPublic,
--   CX_CXXProtected,
--   CX_CXXPrivate
-- };
#c
enum CXXAccessSpecifier
  { CXXInvalidAccessSpecifier = CX_CXXInvalidAccessSpecifier
  , CXXPublic                 = CX_CXXPublic
  , CXXProtected              = CX_CXXProtected
  , CXXPrivate                = CX_CXXPrivate
  };
#endc

{# enum CXXAccessSpecifier {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CX_CXXAccessSpecifier clang_getCXXAccessSpecifier(CXCursor);
{# fun clang_getCXXAccessSpecifier {withVoided* %`Cursor a' } -> `Int' #}
getCXXAccessSpecifier :: Cursor s -> IO CXXAccessSpecifier
getCXXAccessSpecifier c = clang_getCXXAccessSpecifier c >>= return . toEnum

-- unsigned clang_getNumOverloadedDecls(CXCursor cursor);
{# fun clang_getNumOverloadedDecls {withVoided* %`Cursor a' } -> `CUInt' #}
getNumOverloadedDecls :: Cursor s -> IO Int
getNumOverloadedDecls c = clang_getNumOverloadedDecls c >>= return . fromIntegral

-- CXCursor clang_getOverloadedDecl(CXCursor cursor,
--                                                 unsigned index);
{# fun wrapped_clang_getOverloadedDecl as clang_getOverloadedDecl {withVoided* %`Cursor a', `Int' } -> `Ptr (Cursor s)' castPtr #}
getOverloadedDecl :: Proxy s -> Cursor s' -> Int -> IO (Cursor s)
getOverloadedDecl _ c i = clang_getOverloadedDecl c i >>= peek

-- CXType clang_getIBOutletCollectionType(CXCursor);
{# fun wrapped_clang_getIBOutletCollectionType as clang_getIBOutletCollectionType {withVoided* %`Cursor a' } -> `Ptr (Type s)' castPtr #}
getIBOutletCollectionType :: Proxy s -> Cursor s' -> IO (Type s)
getIBOutletCollectionType proxy c = clang_getIBOutletCollectionType c >>= peek


-- We deliberately don't export the constructor for UnsafeCursorList.
-- The only way to unwrap it is registerCursorList.
type CursorList s = DVS.Vector (Cursor s)
instance ClangValueList Cursor
data UnsafeCursorList = UnsafeCursorList !(Ptr ()) !Int

-- void freeCursorList(CXCursor* cursors);
{# fun freeCursorList as freeCursorList' { id `Ptr ()' } -> `()' #}
freeCursorList :: CursorList s -> IO ()
freeCursorList cl = let (ptr, _) = fromCursorList cl in freeCursorList' ptr

registerCursorList :: ClangBase m => IO UnsafeCursorList -> ClangT s m (CursorList s)
registerCursorList action = do
    (_, cursorList) <- clangAllocate (action >>= cursorListToVector) freeCursorList
    return cursorList
{-# INLINEABLE registerCursorList #-}

cursorListToVector :: Storable a => UnsafeCursorList -> IO (DVS.Vector a)
cursorListToVector (UnsafeCursorList cs n) = do
  fptr <- newForeignPtr_ (castPtr cs)
  return $ DVS.unsafeFromForeignPtr fptr 0 n
{-# INLINE cursorListToVector #-}

fromCursorList :: CursorList s -> (Ptr (), Int)
fromCursorList cs = let (p, _, _) = DVS.unsafeToForeignPtr cs in
                   (castPtr $ Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr p, DVS.length cs)

toCursorList :: (Ptr (), Int) -> UnsafeCursorList
toCursorList (cs, n) = UnsafeCursorList cs n

-- A more efficient alternative to clang_visitChildren.
-- void getChildren(CXCursor parent, CXCursor** childrenOut, unsigned* countOut)
{# fun getChildren as getChildren' {withVoided* %`Cursor a', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getChildren :: Cursor s -> IO UnsafeCursorList
unsafe_getChildren c =
 do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
   n <- getChildren' c cxListPtr
   cxList <- peek cxListPtr
   free cxListPtr
   return (toCursorList (cxList, fromIntegral n))

getChildren :: ClangBase m => Cursor s' -> ClangT s m (CursorList s)
getChildren = registerCursorList . unsafe_getChildren

-- Like getChildren, but gets all transitive descendants.
-- void getDescendants(CXCursor parent, CXCursor** childrenOut, unsigned* countOut)
{# fun getDescendants as getDescendants' {withVoided* %`Cursor a', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getDescendants :: Cursor s -> IO UnsafeCursorList
unsafe_getDescendants c =
 do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Cursor s))))
   n <- getDescendants' c cxListPtr
   cxList <- peek cxListPtr
   free cxListPtr
   return (toCursorList (cxList, fromIntegral n))

getDescendants :: ClangBase m => Cursor s' -> ClangT s m (CursorList s)
getDescendants = registerCursorList . unsafe_getDescendants

-- void getDeclarations(CXTranslationUnit tu, CXCursor** declsOut, unsigned* declCountOut);
{# fun getDeclarations as getDeclarations' { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getDeclarations :: TranslationUnit s -> IO UnsafeCursorList
unsafe_getDeclarations t = do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr (Cursor ())))))
   n <- getDeclarations' (unTranslationUnit t) cxListPtr
   cxList <- peek cxListPtr
   free cxListPtr
   return (toCursorList (cxList, fromIntegral n))

getDeclarations :: ClangBase m => TranslationUnit s' -> ClangT s m (CursorList s)
getDeclarations = registerCursorList . unsafe_getDeclarations

-- void getReferences(CXTranslationUnit tu, CXCursor** declsOut, unsigned* declCountOut);
{# fun getReferences as getReferences' { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getReferences :: TranslationUnit s -> IO UnsafeCursorList
unsafe_getReferences t = do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
   n <- getReferences' (unTranslationUnit t) cxListPtr
   cxList <- peek cxListPtr
   free cxListPtr
   return (toCursorList (cxList, fromIntegral n))

getReferences :: ClangBase m => TranslationUnit s' -> ClangT s m (CursorList s)
getReferences = registerCursorList . unsafe_getReferences

-- void getDeclarationsAndReferences(CXTranslationUnit tu,
--                                   CXCursor** declsOut, unsigned* declCountOut,
--                                   CXCursor** refsOut, unsigned* refCountOut);
{# fun getDeclarationsAndReferences as getDeclarationsAndReferences'
     { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek*, id `Ptr (Ptr ())' , alloca- `CUInt' peek* } -> `()' #}
unsafe_getDeclarationsAndReferences :: TranslationUnit s -> IO (UnsafeCursorList, UnsafeCursorList)
unsafe_getDeclarationsAndReferences t =
  alloca (\(declsPtr :: Ptr (Ptr ())) ->
  alloca (\(refsPtr :: Ptr (Ptr ())) -> do
   (nDecls, nRefs) <- getDeclarationsAndReferences' (unTranslationUnit t) (castPtr declsPtr) (castPtr refsPtr)
   decls <- peek declsPtr
   refs <- peek refsPtr
   return ((toCursorList (decls, fromIntegral nDecls)), (toCursorList (refs, fromIntegral nRefs)))))

-- %fun unsafe_getDeclarationsAndReferences :: TranslationUnit s -> IO (UnsafeCursorList, UnsafeCursorList)
-- %call (translationUnit t)
-- %code CXCursor* decls; unsigned declCount;
-- %     CXCursor* refs; unsigned refCount;
-- %     getDeclarationsAndReferences(t, &decls, &declCount, &refs, &refCount);
-- %result (cursorList decls declCount, cursorList refs refCount)

getDeclarationsAndReferences :: ClangBase m => TranslationUnit s'
                             -> ClangT s m (CursorList s, CursorList s)
getDeclarationsAndReferences tu = do
  (ds, rs) <- liftIO $ unsafe_getDeclarationsAndReferences tu
  (,) <$> registerCursorList (return ds) <*> registerCursorList (return rs)

data ParentedCursor s = ParentedCursor
  { parentCursor :: !(Cursor s)
  , childCursor  :: !(Cursor s)
  } deriving (Eq, Ord, Typeable)

instance ClangValue ParentedCursor

instance Storable (ParentedCursor s) where
    sizeOf _ = sizeOfParentedCursor
    {-# INLINE sizeOf #-}

    alignment _ = alignOfParentedCursor
    {-# INLINE alignment #-}

    peek p = do
      parent <- peekByteOff p offsetParentedCursorParent
      child <- peekByteOff p offsetParentedCursorCursor
      return $! ParentedCursor parent child
    {-# INLINE peek #-}

    poke p (ParentedCursor parent child) = do
      pokeByteOff p offsetParentedCursorParent parent
      pokeByteOff p offsetParentedCursorCursor child
    {-# INLINE poke #-}

-- We deliberately don't export the constructor for UnsafeParentedCursorList.
-- The only way to unwrap it is registerParentedCursorList.
type ParentedCursorList s = DVS.Vector (ParentedCursor s)
instance ClangValueList ParentedCursor
data UnsafeParentedCursorList = UnsafeParentedCursorList !(Ptr ()) !Int

-- void freeParentedCursorList(struct ParentedCursor* parentedCursors);
{# fun freeParentedCursorList as freeParentedCursorList' { id `Ptr ()' } -> `()' #}
freeParentedCursorList :: ParentedCursorList s -> IO ()
freeParentedCursorList pcl = let (pclPtr, _ ) = fromParentedCursorList pcl in freeParentedCursorList' pclPtr

registerParentedCursorList :: ClangBase m => IO UnsafeParentedCursorList
                           -> ClangT s m (ParentedCursorList s)
registerParentedCursorList action = do
    (_, pcList) <- clangAllocate (action >>= mkSafe) freeParentedCursorList
    return pcList
  where
    mkSafe (UnsafeParentedCursorList cs n) = do
      fptr <- newForeignPtr_ (castPtr cs)
      return $ DVS.unsafeFromForeignPtr fptr 0 n
{-# INLINEABLE registerParentedCursorList #-}

fromParentedCursorList :: ParentedCursorList s -> (Ptr (), Int)
fromParentedCursorList cs =
  let (p, _, _) = DVS.unsafeToForeignPtr cs in
  (castPtr $ Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr p, DVS.length cs)

toParentedCursorList :: (Ptr (), Int) -> UnsafeParentedCursorList
toParentedCursorList (cs, n) = UnsafeParentedCursorList cs n

-- %dis parentedCursorList cs n = <fromParentedCursorList/toParentedCursorList> (ptr cs) (int n)

-- Like getParentedDescendants, but pairs each descendant with its parent.
-- void getParentedDescendants(CXCursor parent, struct ParentedCursor** descendantsOut,
--                             unsigned* countOut)
{# fun getParentedDescendants as getParentedDescendants' {withVoided* %`Cursor a', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getParentedDescendants :: Cursor s -> IO UnsafeParentedCursorList
unsafe_getParentedDescendants c =
 do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
   n <- getParentedDescendants' c cxListPtr
   cxList <- peek cxListPtr
   free cxListPtr
   return (toParentedCursorList (cxList, fromIntegral n))

getParentedDescendants :: ClangBase m => Cursor s' -> ClangT s m (ParentedCursorList s)
getParentedDescendants = registerParentedCursorList . unsafe_getParentedDescendants

-- void getParentedDeclarations(CXTranslationUnit tu, CXCursor** declsOut, unsigned* declCountOut);
{# fun getParentedDeclarations as getParentedDeclarations' { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getParentedDeclarations :: TranslationUnit s -> IO UnsafeParentedCursorList
unsafe_getParentedDeclarations t = do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr (Cursor ())))))
   n <- getParentedDeclarations' (unTranslationUnit t) (castPtr cxListPtr)
   cxList <- peek cxListPtr
   free cxListPtr
   return (toParentedCursorList (cxList, fromIntegral n))

getParentedDeclarations :: ClangBase m => TranslationUnit s' -> ClangT s m (ParentedCursorList s)
getParentedDeclarations = registerParentedCursorList . unsafe_getParentedDeclarations

-- void getParentedReferences(CXTranslationUnit tu, CXCursor** declsOut, unsigned* declCountOut);
{# fun getParentedReferences as getParentedReferences' { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getParentedReferences :: TranslationUnit s -> IO UnsafeParentedCursorList
unsafe_getParentedReferences t = do
   cxListPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
   n <- getParentedReferences' (unTranslationUnit t) cxListPtr
   cxList <- peek cxListPtr
   free cxListPtr
   return (toParentedCursorList (cxList, fromIntegral n))

getParentedReferences :: ClangBase m => TranslationUnit s' -> ClangT s m (ParentedCursorList s)
getParentedReferences = registerParentedCursorList . unsafe_getParentedReferences

-- void getParentedDeclarationsAndReferences(CXTranslationUnit tu,
--                                           ParentedCursor** declsOut,
--                                           unsigned* declCountOut,
--                                           ParentedCursor** refsOut,
--                                           unsigned* refCountOut);
{# fun getParentedDeclarationsAndReferences as getParentedDeclarationsAndReferences'
     { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek*, id `Ptr (Ptr ())' , alloca- `CUInt' peek* } -> `()' #}
unsafe_getParentedDeclarationsAndReferences :: TranslationUnit s -> IO (UnsafeParentedCursorList, UnsafeParentedCursorList)
unsafe_getParentedDeclarationsAndReferences t = do
   declsPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
   refsPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
   (nDecls, nRefs) <- getParentedDeclarationsAndReferences' (unTranslationUnit t) declsPtr refsPtr
   decls <- peek declsPtr
   refs <- peek refsPtr
   free declsPtr
   free refsPtr
   return ((toParentedCursorList (decls, fromIntegral nDecls)), (toParentedCursorList (refs, fromIntegral nRefs)))

getParentedDeclarationsAndReferences :: ClangBase m => TranslationUnit s'
                                     -> ClangT s m (ParentedCursorList s, ParentedCursorList s)
getParentedDeclarationsAndReferences tu = do
  (ds, rs) <- liftIO $ unsafe_getParentedDeclarationsAndReferences tu
  (,) <$> registerParentedCursorList (return ds) <*> registerParentedCursorList (return rs)

-- CXString clang_getCursorUSR(CXCursor);
{# fun wrapped_clang_getCursorUSR as clang_getCursorUSR {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCursorUSR :: Cursor s -> IO (ClangString ())
unsafe_getCursorUSR c =
  clang_getCursorUSR c >>= peek

getCursorUSR :: ClangBase m => Cursor s' -> ClangT s m (ClangString s)
getCursorUSR = registerClangString . unsafe_getCursorUSR

-- CXString clang_constructUSR_ObjCClass(const char *class_name);
{# fun wrapped_clang_constructUSR_ObjCClass as clang_constructUSR_ObjCClass { `CString' } -> `Ptr (ClangString ())' castPtr #}
unsafe_constructUSR_ObjCClass :: String -> IO (ClangString ())
unsafe_constructUSR_ObjCClass s = withCString s (\sPtr -> clang_constructUSR_ObjCClass sPtr >>= peek)

constructUSR_ObjCClass :: ClangBase m => String -> ClangT s m (ClangString s)
constructUSR_ObjCClass = registerClangString . unsafe_constructUSR_ObjCClass

-- CXString
--   clang_constructUSR_ObjCCategory(const char *class_name,
--                                  const char *category_name);
{# fun wrapped_clang_constructUSR_ObjCCategory as clang_constructUSR_ObjCCategory { `CString', `CString' } -> `Ptr (ClangString ())' castPtr #}
unsafe_constructUSR_ObjCCategory :: String -> String -> IO (ClangString ())
unsafe_constructUSR_ObjCCategory s p =
  withCString s (\sPtr ->
  withCString p (\pPtr ->
    clang_constructUSR_ObjCCategory sPtr pPtr >>= peek))

constructUSR_ObjCCategory :: ClangBase m => String -> String -> ClangT s m (ClangString s)
constructUSR_ObjCCategory = (registerClangString .) . unsafe_constructUSR_ObjCCategory

-- CXString
--   clang_constructUSR_ObjCProtocol(const char *protocol_name);
{# fun wrapped_clang_constructUSR_ObjCProtocol as clang_constructUSR_ObjCProtocol { `CString' } -> `Ptr (ClangString ())' castPtr #}
unsafe_constructUSR_ObjCProtocol :: String -> IO (ClangString ())
unsafe_constructUSR_ObjCProtocol s = withCString s (\sPtr -> clang_constructUSR_ObjCProtocol sPtr >>= peek)

constructUSR_ObjCProtocol :: ClangBase m => String -> ClangT s m (ClangString s)
constructUSR_ObjCProtocol = registerClangString . unsafe_constructUSR_ObjCProtocol

-- CXString clang_constructUSR_ObjCIvar(const char *name,
--                                                     CXString classUSR);
{# fun wrapped_clang_constructUSR_ObjCIvar as clang_constructUSR_ObjCIvar { `CString' , id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_constructUSR_ObjCIvar :: String -> Ptr () -> Word32 -> IO (ClangString ())
unsafe_constructUSR_ObjCIvar s d f =
  let clangString = ClangString  d f in
  withCString s (\sPtr -> new clangString >>= \cs -> clang_constructUSR_ObjCIvar sPtr (castPtr cs) >>= peek)

constructUSR_ObjCIvar :: ClangBase m => String -> ClangString s' -> ClangT s m (ClangString s)
constructUSR_ObjCIvar s (ClangString d f) = registerClangString $ unsafe_constructUSR_ObjCIvar s d f

-- CXString clang_constructUSR_ObjCMethod(const char *name,
--                                                       unsigned isInstanceMethod,
--                                                       CXString classUSR);
{# fun wrapped_clang_constructUSR_ObjCMethod as clang_constructUSR_ObjCMethod { `CString' , `CUInt' , id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_constructUSR_ObjCMethod :: String -> Bool -> Ptr () -> Word32 -> IO (ClangString ())
unsafe_constructUSR_ObjCMethod s b d f =
  let clangString = ClangString  d f in
  withCString s (\sPtr -> (new clangString >>= \cs -> clang_constructUSR_ObjCMethod sPtr (fromIntegral ((fromBool b) :: Int)) (castPtr cs) >>= peek))

constructUSR_ObjCMethod :: ClangBase m => String -> Bool -> ClangString s' -> ClangT s m (ClangString s)
constructUSR_ObjCMethod s b (ClangString d f) =
  registerClangString $ unsafe_constructUSR_ObjCMethod s b d f

-- CXString clang_constructUSR_ObjCProperty(const char *property,
--                                                         CXString classUSR);
{# fun wrapped_clang_constructUSR_ObjCProperty as clang_constructUSR_ObjCProperty { `CString' , id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_constructUSR_ObjCProperty :: String -> Ptr () -> Word32 -> IO (ClangString ())
unsafe_constructUSR_ObjCProperty s d f =
  let clangString = ClangString  d f in
  withCString s (\sPtr -> (new clangString >>= \cs -> clang_constructUSR_ObjCProperty sPtr (castPtr cs) >>= peek))

constructUSR_ObjCProperty :: ClangBase m => String -> ClangString s' -> ClangT s m (ClangString s)
constructUSR_ObjCProperty s (ClangString d f) =
  registerClangString $ unsafe_constructUSR_ObjCProperty s d f

-- CXString clang_getCursorSpelling(CXCursor);
{# fun wrapped_clang_getCursorSpelling as clang_getCursorSpelling {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCursorSpelling :: Cursor s -> IO (ClangString ())
unsafe_getCursorSpelling c =
  clang_getCursorSpelling c >>= peek

getCursorSpelling :: ClangBase m => Cursor s' -> ClangT s m (ClangString s)
getCursorSpelling = registerClangString . unsafe_getCursorSpelling

-- CXSourceRange clang_Cursor_getSpellingNameRange(CXCursor,
--                                                 unsigned pieceIndex,
--                                                 unsigned options);
{# fun wrapped_clang_Cursor_getSpellingNameRange as clang_Cursor_getSpellingNameRange {withVoided* %`Cursor a', `Int', `Int' } -> `Ptr (SourceRange s)' castPtr #}
cursor_getSpellingNameRange :: Proxy s -> Cursor s' -> Int -> IO (SourceRange s)
cursor_getSpellingNameRange _ c i = clang_Cursor_getSpellingNameRange c i 0 >>= peek

-- CXString clang_getCursorDisplayName(CXCursor);
{# fun wrapped_clang_getCursorDisplayName as clang_getCursorDisplayName {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCursorDisplayName :: Cursor s -> IO (ClangString ())
unsafe_getCursorDisplayName c =
  clang_getCursorDisplayName c >>= peek

getCursorDisplayName :: ClangBase m => Cursor s' -> ClangT s m (ClangString s)
getCursorDisplayName = registerClangString . unsafe_getCursorDisplayName

-- CXCursor clang_getCursorReferenced(CXCursor);
{# fun wrapped_clang_getCursorReferenced as clang_getCursorReferenced {withVoided* %`Cursor a' } -> `Ptr (Cursor s)' castPtr #}
getCursorReferenced :: Proxy s -> Cursor s' -> IO (Cursor s)
getCursorReferenced _ c =
  clang_getCursorReferenced c >>= peek

-- CXCursor clang_getCursorDefinition(CXCursor);
{# fun wrapped_clang_getCursorDefinition as clang_getCursorDefinition {withVoided* %`Cursor a' } -> `Ptr (Cursor s)' castPtr #}
getCursorDefinition :: Proxy s -> Cursor s' -> IO (Cursor s)
getCursorDefinition _ c = clang_getCursorDefinition c >>= peek

-- unsigned clang_isCursorDefinition(CXCursor);
{# fun clang_isCursorDefinition {withVoided* %`Cursor a' } -> `Bool' toBool #}
isCursorDefinition :: Cursor s -> IO Bool
isCursorDefinition c = clang_isCursorDefinition c

-- unsigned clang_Cursor_isDynamicCall(CXCursor);
{# fun clang_Cursor_isDynamicCall {withVoided* %`Cursor a' } -> `Bool' toBool #}
cursor_isDynamicCall :: Cursor s -> IO Bool
cursor_isDynamicCall c = clang_Cursor_isDynamicCall c

-- CXCursor clang_getCanonicalCursor(CXCursor);
{# fun wrapped_clang_getCanonicalCursor as clang_getCanonicalCursor {withVoided* %`Cursor a' } -> `Ptr (Cursor s)' castPtr #}
getCanonicalCursor :: Proxy s -> Cursor s' -> IO (Cursor s)
getCanonicalCursor _ c =
  clang_getCanonicalCursor c >>= peek

-- int clang_Cursor_getObjCSelectorIndex(CXCursor);
{# fun clang_Cursor_getObjCSelectorIndex {withVoided* %`Cursor a' } -> `Int'  #}
cursor_getObjCSelectorIndex :: Cursor s -> IO Int
cursor_getObjCSelectorIndex c = clang_Cursor_getObjCSelectorIndex c

-- CXType clang_Cursor_getReceiverType(CXCursor C);
{# fun wrapped_clang_Cursor_getReceiverType as clang_Cursor_getReceiverType {withVoided* %`Cursor a' } -> `Ptr (Type s)' castPtr #}
cursor_getReceiverType :: Proxy s -> Cursor s' -> IO (Type s)
cursor_getReceiverType proxy c =
  clang_Cursor_getReceiverType c >>= peek

-- typedef enum {
--   CXObjCPropertyAttr_noattr    = 0x00,
--   CXObjCPropertyAttr_readonly  = 0x01,
--   CXObjCPropertyAttr_getter    = 0x02,
--   CXObjCPropertyAttr_assign    = 0x04,
--   CXObjCPropertyAttr_readwrite = 0x08,
--   CXObjCPropertyAttr_retain    = 0x10,
--   CXObjCPropertyAttr_copy      = 0x20,
--   CXObjCPropertyAttr_nonatomic = 0x40,
--   CXObjCPropertyAttr_setter    = 0x80,
--   CXObjCPropertyAttr_atomic    = 0x100,
--   CXObjCPropertyAttr_weak      = 0x200,
--   CXObjCPropertyAttr_strong    = 0x400,
--   CXObjCPropertyAttr_unsafe_unretained = 0x800
-- } CXObjCPropertyAttrKind;

#c
enum ObjCPropertyAttrKind
  { ObjCPropertyAttr_noattr           = CXObjCPropertyAttr_noattr
  , ObjCPropertyAttr_readonly         = CXObjCPropertyAttr_readonly
  , ObjCPropertyAttr_getter           = CXObjCPropertyAttr_getter
  , ObjCPropertyAttr_assign           = CXObjCPropertyAttr_assign
  , ObjCPropertyAttr_readwrite        = CXObjCPropertyAttr_readwrite
  , ObjCPropertyAttr_retain           = CXObjCPropertyAttr_retain
  , ObjCPropertyAttr_copy             = CXObjCPropertyAttr_copy
  , ObjCPropertyAttr_nonatomic        = CXObjCPropertyAttr_nonatomic
  , ObjCPropertyAttr_setter           = CXObjCPropertyAttr_setter
  , ObjCPropertyAttr_atomic           = CXObjCPropertyAttr_atomic
  , ObjCPropertyAttr_weak             = CXObjCPropertyAttr_weak
  , ObjCPropertyAttr_strong           = CXObjCPropertyAttr_strong
  , ObjCPropertyAttr_unsafe_unretained= CXObjCPropertyAttr_unsafe_unretained
 };
#endc
{# enum ObjCPropertyAttrKind {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags ObjCPropertyAttrKind where
  toBit ObjCPropertyAttr_noattr            = 0x0
  toBit ObjCPropertyAttr_readonly          = 0x1
  toBit ObjCPropertyAttr_getter            = 0x2
  toBit ObjCPropertyAttr_assign            = 0x4
  toBit ObjCPropertyAttr_readwrite         = 0x8
  toBit ObjCPropertyAttr_retain            = 0x10
  toBit ObjCPropertyAttr_copy              = 0x20
  toBit ObjCPropertyAttr_nonatomic         = 0x40
  toBit ObjCPropertyAttr_setter            = 0x80
  toBit ObjCPropertyAttr_atomic            = 0x100
  toBit ObjCPropertyAttr_weak              = 0x200
  toBit ObjCPropertyAttr_strong            = 0x400
  toBit ObjCPropertyAttr_unsafe_unretained = 0x800

-- unsigned clang_Cursor_getObjCPropertyAttributes(CXCursor C, unsigned reserved);
{# fun clang_Cursor_getObjCPropertyAttributes {withVoided* %`Cursor a', `Int' } -> `CUInt' #}
cursor_getObjCPropertyAttributes :: Cursor s -> IO Int
cursor_getObjCPropertyAttributes c = clang_Cursor_getObjCPropertyAttributes c 0 >>= return . fromIntegral

-- typedef enum {
--   CXObjCDeclQualifier_None = 0x0,
--   CXObjCDeclQualifier_In = 0x1,
--   CXObjCDeclQualifier_Inout = 0x2,
--   CXObjCDeclQualifier_Out = 0x4,
--   CXObjCDeclQualifier_Bycopy = 0x8,
--   CXObjCDeclQualifier_Byref = 0x10,
--   CXObjCDeclQualifier_Oneway = 0x20
-- } CXObjCDeclQualifierKind;

#c
enum ObjCDeclQualifierKind
  { ObjCDeclQualifier_None   = CXObjCDeclQualifier_None
  , ObjCDeclQualifier_In     = CXObjCDeclQualifier_In
  , ObjCDeclQualifier_Inout  = CXObjCDeclQualifier_Inout
  , ObjCDeclQualifier_Out    = CXObjCDeclQualifier_Out
  , ObjCDeclQualifier_Bycopy = CXObjCDeclQualifier_Bycopy
  , ObjCDeclQualifier_Byref  = CXObjCDeclQualifier_Byref
  , ObjCDeclQualifier_Oneway = CXObjCDeclQualifier_Oneway
 };
#endc
{# enum ObjCDeclQualifierKind {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags ObjCDeclQualifierKind where
  toBit ObjCDeclQualifier_None   = 0x0
  toBit ObjCDeclQualifier_In     = 0x1
  toBit ObjCDeclQualifier_Inout  = 0x2
  toBit ObjCDeclQualifier_Out    = 0x4
  toBit ObjCDeclQualifier_Bycopy = 0x8
  toBit ObjCDeclQualifier_Byref  = 0x10
  toBit ObjCDeclQualifier_Oneway = 0x20

-- unsigned clang_Cursor_getObjCDeclQualifiers(CXCursor C);
{# fun clang_Cursor_getObjCDeclQualifiers {withVoided* %`Cursor a' } -> `CUInt' #}
cursor_getObjCDeclQualifiers :: Cursor s -> IO Int
cursor_getObjCDeclQualifiers c =
  clang_Cursor_getObjCDeclQualifiers c >>= return . fromIntegral

-- unsigned clang_Cursor_isObjCOptional(CXCursor);
{# fun clang_Cursor_isObjCOptional {withVoided* %`Cursor a' } -> `Bool' toBool #}
cursor_isObjCOptional :: Cursor s -> IO Bool
cursor_isObjCOptional c = clang_Cursor_isObjCOptional c

-- unsigned clang_Cursor_isVariadic(CXCursor);
{# fun clang_Cursor_isVariadic {withVoided* %`Cursor a' } -> `Bool' toBool #}
cursor_isVariadic :: Cursor s -> IO Bool
cursor_isVariadic c = clang_Cursor_isVariadic c

-- CXSourceRange clang_Cursor_getCommentRange(CXCursor C);
{# fun wrapped_clang_Cursor_getCommentRange as clang_Cursor_getCommentRange {withVoided* %`Cursor a' } -> `Ptr (SourceRange s)' castPtr #}
cursor_getCommentRange :: Proxy s -> Cursor s' -> IO (SourceRange s)
cursor_getCommentRange _ c = clang_Cursor_getCommentRange c >>= peek

-- CXString clang_Cursor_getRawCommentText(CXCursor C);
{# fun wrapped_clang_Cursor_getRawCommentText as clang_Cursor_getRawCommentText {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_Cursor_getRawCommentText :: Cursor s -> IO (ClangString ())
unsafe_Cursor_getRawCommentText c =
  clang_Cursor_getRawCommentText c >>= peek

cursor_getRawCommentText :: ClangBase m => Cursor s' -> ClangT s m (ClangString s)
cursor_getRawCommentText = registerClangString . unsafe_Cursor_getRawCommentText

-- CXString clang_Cursor_getBriefCommentText(CXCursor C);
{# fun wrapped_clang_Cursor_getBriefCommentText as clang_Cursor_getBriefCommentText {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_Cursor_getBriefCommentText :: Cursor s -> IO (ClangString ())
unsafe_Cursor_getBriefCommentText c =
  clang_Cursor_getBriefCommentText c >>= peek

cursor_getBriefCommentText :: ClangBase m => Cursor s' -> ClangT s m (ClangString s)
cursor_getBriefCommentText = registerClangString . unsafe_Cursor_getBriefCommentText

-- CXComment clang_Cursor_getParsedComment(CXCursor C);

{# fun wrapped_clang_Cursor_getParsedComment as clang_Cursor_getParsedComment {withVoided* %`Cursor a' } -> `Ptr (Comment s)' castPtr #}
cursor_getParsedComment :: Proxy s -> Cursor s' -> IO (Comment s)
cursor_getParsedComment _ c =
  clang_Cursor_getParsedComment c >>= peek

-- typedef void *CXModule;
newtype Module s = Module { unModule :: Ptr () }
                   deriving (Eq, Ord, Typeable)

instance ClangValue Module

-- %dis cxmodule p = <unModule/Module> (ptr p)

maybeModule :: Module s' -> Maybe (Module s)
maybeModule (Module p) | p == nullPtr = Nothing
maybeModule f                         = Just (unsafeCoerce f)

unMaybeModule :: Maybe (Module s') -> Module s
unMaybeModule (Just f) = unsafeCoerce f
unMaybeModule Nothing  = Module nullPtr

-- CXModule clang_Cursor_getModule(CXCursor C);
{# fun clang_Cursor_getModule {withVoided* %`Cursor a' } -> `Ptr ()' id #}
cursor_getModule :: Proxy s -> Cursor s' -> IO (Module s)
cursor_getModule _ c =
  clang_Cursor_getModule c >>= return . Module

-- CXFile clang_Module_getASTFile(CXModule Module);
{# fun clang_Module_getASTFile { id `Ptr ()' } -> `Ptr ()' id #}
module_getASTFile :: Proxy s -> Module s' -> IO (File s)
module_getASTFile _ m = clang_Module_getASTFile (unModule m) >>= return . File

-- CXModule clang_Module_getParent(CXModule Module);
{# fun clang_Module_getParent { id `Ptr ()' } -> `Ptr ()' id #}
module_getParent :: Proxy s -> Module s' -> IO (Maybe (Module s))
module_getParent _ m = clang_Module_getParent (unModule m) >>= return . maybeModule . Module

-- CXString clang_Module_getName(CXModule Module);
{# fun wrapped_clang_Module_getName as clang_Module_getName { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_Module_getName :: Module s -> IO (ClangString ())
unsafe_Module_getName m = clang_Module_getName (unModule m) >>= peek

module_getName :: ClangBase m => Module s' -> ClangT s m (ClangString s)
module_getName = registerClangString . unsafe_Module_getName

-- CXString clang_Module_getFullName(CXModule Module);
{# fun wrapped_clang_Module_getFullName as clang_Module_getFullName { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_Module_getFullName :: Module s -> IO (ClangString ())
unsafe_Module_getFullName m = clang_Module_getFullName (unModule m) >>= peek

module_getFullName :: ClangBase m => Module s' -> ClangT s m (ClangString s)
module_getFullName = registerClangString . unsafe_Module_getFullName

-- unsigned clang_Module_getNumTopLevelHeaders(CXTranslationUnit, CXModule Module);
{# fun clang_Module_getNumTopLevelHeaders { id `Ptr ()', id `Ptr ()' } -> `Int' #}
module_getNumTopLevelHeaders :: TranslationUnit s -> Module s' -> IO Int
module_getNumTopLevelHeaders t m = clang_Module_getNumTopLevelHeaders (unTranslationUnit t) (unModule m)

-- CXFile clang_Module_getTopLevelHeader(CXTranslationUnit, CXModule Module, unsigned Index);
{# fun clang_Module_getTopLevelHeader { id `Ptr ()', id `Ptr ()', `Int' } -> `Ptr ()' id #}
module_getTopLevelHeader :: Proxy s -> TranslationUnit s' -> Module s'' -> Int -> IO (File s)
module_getTopLevelHeader _ t m i = clang_Module_getTopLevelHeader (unTranslationUnit t) (unModule m) i >>= return . File

-- unsigned clang_CXXMethod_isPureVirtual(CXCursor C);
{# fun clang_CXXMethod_isPureVirtual {withVoided* %`Cursor a' } -> `Bool' toBool #}
cXXMethod_isPureVirtual :: Cursor s -> IO Bool
cXXMethod_isPureVirtual c = clang_Cursor_isBitField c

-- unsigned clang_CXXMethod_isStatic(CXCursor C);
{# fun clang_CXXMethod_isStatic {withVoided* %`Cursor a' } -> `Bool' toBool #}
cXXMethod_isStatic :: Cursor s -> IO Bool
cXXMethod_isStatic c = clang_Cursor_isBitField c

-- unsigned clang_CXXMethod_isVirtual(CXCursor C);
{# fun clang_CXXMethod_isVirtual {withVoided* %`Cursor a' } -> `Bool' toBool #}
cXXMethod_isVirtual :: Cursor s -> IO Bool
cXXMethod_isVirtual c = clang_Cursor_isBitField c

-- enum CXCursorKind clang_getTemplateCursorKind(CXCursor C);
{# fun clang_getTemplateCursorKind {withVoided* %`Cursor a' } -> `Int' #}
getTemplateCursorKind :: Cursor s -> IO CursorKind
getTemplateCursorKind c =
  clang_getTemplateCursorKind c >>= return . toEnum

-- CXCursor clang_getSpecializedCursorTemplate(CXCursor C);
{# fun wrapped_clang_getSpecializedCursorTemplate as clang_getSpecializedCursorTemplate {withVoided* %`Cursor a' } -> `Ptr (Cursor s)' castPtr #}
getSpecializedCursorTemplate :: Proxy s -> Cursor s' -> IO (Cursor s)
getSpecializedCursorTemplate _ c =
  clang_getSpecializedCursorTemplate c >>= peek

-- CXSourceRange clang_getCursorReferenceNameRange(CXCursor C,
--                                                 unsigned NameFlags,
--                                                 unsigned PieceIndex);
{# fun wrapped_clang_getCursorReferenceNameRange as clang_getCursorReferenceNameRange {withVoided* %`Cursor a', `Int' , `Int' } -> `Ptr (SourceRange s)' castPtr #}
getCursorReferenceNameRange :: Proxy s -> Cursor s' -> Int -> Int -> IO (SourceRange s)
getCursorReferenceNameRange _ c fs idx = clang_getCursorReferenceNameRange c fs idx >>= peek

-- enum CXNameRefFlags {
--   CXNameRange_WantQualifier = 0x1,
--   CXNameRange_WantTemplateArgs = 0x2,
--   CXNameRange_WantSinglePiece = 0x4
-- };
#c
enum NameRefFlags
  { NameRange_WantQualifier    = CXNameRange_WantQualifier
  , NameRange_WantTemplateArgs = CXNameRange_WantTemplateArgs
  , NameRange_WantSinglePiece  = CXNameRange_WantSinglePiece
  };
#endc
{# enum NameRefFlags {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags NameRefFlags where
  toBit NameRange_WantQualifier    = 0x01
  toBit NameRange_WantTemplateArgs = 0x02
  toBit NameRange_WantSinglePiece  = 0x04

-- enum CXCommentKind {
--   CXComment_Null = 0,
--   CXComment_Text = 1,
--   CXComment_InlineCommand = 2,
--   CXComment_HTMLStartTag = 3,
--   CXComment_HTMLEndTag = 4,
--   CXComment_Paragraph = 5,
--   CXComment_BlockCommand = 6,
--   CXComment_ParamCommand = 7,
--   CXComment_TParamCommand = 8,
--   CXComment_VerbatimBlockCommand = 9,
--   CXComment_VerbatimBlockLine = 10,
--   CXComment_VerbatimLine = 11,
--   CXComment_FullComment = 12
-- };
#c
enum CommentKind
  { NullComment                 = CXComment_Null
  , TextComment                 = CXComment_Text
  , InlineCommandComment        = CXComment_InlineCommand
  , HTMLStartTagComment         = CXComment_HTMLStartTag
  , HTMLEndTagComment           = CXComment_HTMLEndTag
  , ParagraphComment            = CXComment_Paragraph
  , BlockCommandComment         = CXComment_BlockCommand
  , ParamCommandComment         = CXComment_ParamCommand
  , TParamCommandComment        = CXComment_TParamCommand
  , VerbatimBlockCommandComment = CXComment_VerbatimBlockCommand
  , VerbatimBlockLineComment    = CXComment_VerbatimBlockLine
  , VerbatimLineComment         = CXComment_VerbatimLine
  , FullComment                 = CXComment_FullComment
 };
#endc
{# enum CommentKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CXCommentInlineCommandRenderKind {
--   CXCommentInlineCommandRenderKind_Normal,
--   CXCommentInlineCommandRenderKind_Bold,
--   CXCommentInlineCommandRenderKind_Monospaced,
--   CXCommentInlineCommandRenderKind_Emphasized
-- };

-- | A rendering style which should be used for the associated inline command in the comment AST.
#c
enum InlineCommandRenderStyle
  { NormalInlineCommandRenderStyle     = CXCommentInlineCommandRenderKind_Normal
  , BoldInlineCommandRenderStyle       = CXCommentInlineCommandRenderKind_Bold
  , MonospacedInlineCommandRenderStyle = CXCommentInlineCommandRenderKind_Monospaced
  , EmphasizedInlineCommandRenderStyle = CXCommentInlineCommandRenderKind_Emphasized
 };
#endc
{# enum InlineCommandRenderStyle {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}
-- enum CXCommentParamPassDirection {
--   CXCommentParamPassDirection_In,
--   CXCommentParamPassDirection_Out,
--   CXCommentParamPassDirection_InOut
-- };

-- | A parameter passing direction.
#c
enum ParamPassDirectionKind
  { InParamPassDirection    = CXCommentParamPassDirection_In
  , OutParamPassDirection   = CXCommentParamPassDirection_Out
  , InOutParamPassDirection = CXCommentParamPassDirection_InOut
  };
#endc
{#enum ParamPassDirectionKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CXCommentKind clang_Comment_getKind(CXComment Comment);
{# fun clang_Comment_getKind {withVoided* %`Comment a' } -> `Int' #}
comment_getKind :: Comment s -> IO CommentKind
comment_getKind c =
  clang_Comment_getKind c >>= return . toEnum

-- unsigned clang_Comment_getNumChildren(CXComment Comment);
{# fun clang_Comment_getNumChildren {withVoided* %`Comment a' } -> `Int' #}
comment_getNumChildren :: Comment s -> IO Int
comment_getNumChildren c =
  clang_Comment_getNumChildren c

-- CXComment clang_Comment_getChild(CXComment Comment, unsigned ChildIdx);
{# fun clang_Comment_getChild {withVoided* %`Comment a', `Int' } -> `Ptr (Comment s)' castPtr #}
comment_getChild :: Proxy s -> Comment s' -> Int -> IO (Comment s)
comment_getChild _ c i = clang_Comment_getChild c i >>= peek

-- unsigned clang_Comment_isWhitespace(CXComment Comment);
{# fun clang_Comment_isWhitespace {withVoided* %`Comment a' } -> `Bool' toBool #}
comment_isWhitespace :: Comment s -> IO Bool
comment_isWhitespace c = clang_Comment_isWhitespace c

-- unsigned clang_InlineContentComment_hasTrailingNewline(CXComment Comment);
{# fun clang_InlineContentComment_hasTrailingNewline {withVoided* %`Comment a' } -> `Bool' toBool #}
inlineContentComment_hasTrailingNewline :: Comment s -> IO Bool
inlineContentComment_hasTrailingNewline c = clang_InlineContentComment_hasTrailingNewline c

-- CXString clang_TextComment_getText(CXComment Comment);
{# fun wrapped_clang_TextComment_getText as clang_TextComment_getText {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_TextComment_getText :: Comment s -> IO (ClangString ())
unsafe_TextComment_getText c =
  clang_TextComment_getText c >>= peek

textComment_getText :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
textComment_getText = registerClangString . unsafe_TextComment_getText

-- CXString clang_InlineCommandComment_getCommandName(CXComment Comment);
{# fun clang_InlineCommandComment_getCommandName {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_InlineCommandComment_getCommandName :: Comment s -> IO (ClangString ())
unsafe_InlineCommandComment_getCommandName c =
  clang_InlineCommandComment_getCommandName c >>= peek

inlineCommandComment_getCommandName :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
inlineCommandComment_getCommandName = registerClangString . unsafe_InlineCommandComment_getCommandName

-- enum CXCommentInlineCommandRenderKind clang_InlineCommandComment_getRenderKind(CXComment Comment);
{# fun clang_InlineCommandComment_getRenderKind {withVoided* %`Comment a' } -> `Int' #}
inlineCommandComment_getRenderKind :: Comment s -> IO InlineCommandRenderStyle
inlineCommandComment_getRenderKind c =
  clang_InlineCommandComment_getRenderKind c >>= return . toEnum

-- unsigned clang_InlineCommandComment_getNumArgs(CXComment Comment);
{# fun clang_InlineCommandComment_getNumArgs {withVoided* %`Comment a' } -> `Int' #}
inlineCommandComment_getNumArgs :: Comment s -> IO Int
inlineCommandComment_getNumArgs c =
  clang_InlineCommandComment_getNumArgs c

-- CXString clang_InlineCommandComment_getArgText(CXComment Comment, unsigned ArgIdx);
{# fun clang_InlineCommandComment_getArgText {withVoided* %`Comment a', `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_InlineCommandComment_getArgText :: Comment s -> Int -> IO (ClangString ())
unsafe_InlineCommandComment_getArgText c idx =
  clang_InlineCommandComment_getArgText c idx >>= peek

inlineCommandComment_getArgText :: ClangBase m => Comment s' -> Int -> ClangT s m (ClangString s)
inlineCommandComment_getArgText = (registerClangString .) . unsafe_InlineCommandComment_getArgText

-- CXString clang_HTMLTagComment_getTagName(CXComment Comment);
{# fun wrapped_clang_HTMLTagComment_getTagName as clang_HTMLTagComment_getTagName {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_HTMLTagComment_getTagName :: Comment s -> IO (ClangString ())
unsafe_HTMLTagComment_getTagName c =
  clang_HTMLTagComment_getTagName c >>= peek

hTMLTagComment_getTagName :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
hTMLTagComment_getTagName = registerClangString . unsafe_HTMLTagComment_getTagName

-- unsigned clang_HTMLStartTagComment_isSelfClosing(CXComment Comment);
{# fun clang_HTMLStartTagComment_isSelfClosing {withVoided* %`Comment a' } -> `Bool' toBool #}
hTMLStartTagComment_isSelfClosing :: Comment s -> IO Bool
hTMLStartTagComment_isSelfClosing c = clang_HTMLStartTagComment_isSelfClosing c

-- unsigned clang_HTMLStartTag_getNumAttrs(CXComment Comment);
{# fun clang_HTMLStartTag_getNumAttrs {withVoided* %`Comment a' } -> `Int' #}
hTMLStartTag_getNumAttrs :: Comment s -> IO Int
hTMLStartTag_getNumAttrs c =
  clang_HTMLStartTag_getNumAttrs c

-- CXString clang_HTMLStartTag_getAttrName(CXComment Comment, unsigned AttrIdx);
{# fun clang_HTMLStartTag_getAttrName {withVoided* %`Comment a', `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_HTMLStartTag_getAttrName :: Comment s -> Int -> IO (ClangString ())
unsafe_HTMLStartTag_getAttrName c idx =
  clang_HTMLStartTag_getAttrName c idx >>= peek

hTMLStartTag_getAttrName :: ClangBase m => Comment s' -> Int -> ClangT s m (ClangString s)
hTMLStartTag_getAttrName = (registerClangString .) . unsafe_HTMLStartTag_getAttrName

-- CXString clang_HTMLStartTag_getAttrValue(CXComment Comment, unsigned AttrIdx);
{# fun clang_HTMLStartTag_getAttrValue {withVoided* %`Comment a', `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_HTMLStartTag_getAttrValue :: Comment s -> Int -> IO (ClangString ())
unsafe_HTMLStartTag_getAttrValue c idx =
  clang_HTMLStartTag_getAttrValue c idx >>= peek

hTMLStartTag_getAttrValue :: ClangBase m => Comment s' -> Int -> ClangT s m (ClangString s)
hTMLStartTag_getAttrValue = (registerClangString .) . unsafe_HTMLStartTag_getAttrValue

-- CXString clang_BlockCommandComment_getCommandName(CXComment Comment);
{# fun clang_BlockCommandComment_getCommandName {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_BlockCommandComment_getCommandName :: Comment s -> IO (ClangString ())
unsafe_BlockCommandComment_getCommandName c =
  clang_BlockCommandComment_getCommandName c >>= peek

blockCommandComment_getCommandName :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
blockCommandComment_getCommandName = registerClangString . unsafe_BlockCommandComment_getCommandName

-- unsigned clang_BlockCommandComment_getNumArgs(CXComment Comment);
{# fun clang_BlockCommandComment_getNumArgs {withVoided* %`Comment a' } -> `Int' #}
blockCommandComment_getNumArgs :: Comment s -> IO Int
blockCommandComment_getNumArgs c =
  clang_BlockCommandComment_getNumArgs c

-- CXString clang_BlockCommandComment_getArgText(CXComment Comment, unsigned ArgIdx);
{# fun clang_BlockCommandComment_getArgText {withVoided* %`Comment a', `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_BlockCommandComment_getArgText :: Comment s -> Int -> IO (ClangString ())
unsafe_BlockCommandComment_getArgText c idx =
  clang_BlockCommandComment_getArgText c idx >>= peek

blockCommandComment_getArgText :: ClangBase m => Comment s' -> Int -> ClangT s m (ClangString s)
blockCommandComment_getArgText = (registerClangString .) . unsafe_BlockCommandComment_getArgText

-- CXComment clang_BlockCommandComment_getParagraph(CXComment Comment);
{# fun clang_BlockCommandComment_getParagraph {withVoided* %`Comment a' } -> `Ptr (Comment s)' castPtr #}
blockCommandComment_getParagraph :: Proxy s -> Comment s' -> IO (Comment s)
blockCommandComment_getParagraph _ c =
  clang_BlockCommandComment_getParagraph c >>= peek

-- CXString clang_ParamCommandComment_getParamName(CXComment Comment);
{# fun clang_ParamCommandComment_getParamName {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_ParamCommandComment_getParamName :: Comment s -> IO (ClangString ())
unsafe_ParamCommandComment_getParamName c =
  clang_ParamCommandComment_getParamName c >>= peek

paramCommandComment_getParamName :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
paramCommandComment_getParamName = registerClangString . unsafe_ParamCommandComment_getParamName

-- unsigned clang_ParamCommandComment_isParamIndexValid(CXComment Comment);
{# fun clang_ParamCommandComment_isParamIndexValid {withVoided* %`Comment a' } -> `Bool' toBool #}
paramCommandComment_isParamIndexValid :: Comment s -> IO Bool
paramCommandComment_isParamIndexValid c =
  clang_ParamCommandComment_isParamIndexValid c

-- unsigned clang_ParamCommandComment_getParamIndex(CXComment Comment);
{# fun clang_ParamCommandComment_getParamIndex {withVoided* %`Comment a' } -> `Int' #}
paramCommandComment_getParamIndex :: Comment s -> IO Int
paramCommandComment_getParamIndex c =
  clang_ParamCommandComment_getParamIndex c

-- unsigned clang_ParamCommandComment_isDirectionExplicit(CXComment Comment);
{# fun clang_ParamCommandComment_isDirectionExplicit {withVoided* %`Comment a' } -> `Bool' toBool #}
paramCommandComment_isDirectionExplicit :: Comment s -> IO Bool
paramCommandComment_isDirectionExplicit c =
  clang_ParamCommandComment_isDirectionExplicit c

-- enum CXCommentParamPassDirection clang_ParamCommandComment_getDirection(CXComment Comment);
{# fun clang_ParamCommandComment_getDirection {withVoided* %`Comment a' } -> `Int' #}
paramCommandComment_getDirection :: Comment s -> IO ParamPassDirectionKind
paramCommandComment_getDirection c =
  clang_ParamCommandComment_getDirection c >>= return . toEnum

-- CXString clang_TParamCommandComment_getParamName(CXComment Comment);
{# fun clang_TParamCommandComment_getParamName {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_TParamCommandComment_getParamName :: Comment s -> IO (ClangString ())
unsafe_TParamCommandComment_getParamName c =
  clang_TParamCommandComment_getParamName c >>= peek

tParamCommandComment_getParamName :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
tParamCommandComment_getParamName = registerClangString . unsafe_TParamCommandComment_getParamName

-- unsigned clang_TParamCommandComment_isParamPositionValid(CXComment Comment);
{# fun clang_TParamCommandComment_isParamPositionValid {withVoided* %`Comment a' } -> `Bool' toBool #}
tParamCommandComment_isParamPositionValid :: Comment s -> IO Bool
tParamCommandComment_isParamPositionValid c =
  clang_TParamCommandComment_isParamPositionValid c

-- unsigned clang_TParamCommandComment_getDepth(CXComment Comment);
{# fun clang_TParamCommandComment_getDepth { withVoided* %`Comment a' } -> `Int' #}
tParamCommandComment_getDepth :: Comment s -> IO Int
tParamCommandComment_getDepth c =
  clang_TParamCommandComment_getDepth c

-- unsigned clang_TParamCommandComment_getIndex(CXComment Comment, unsigned Depth);
{# fun clang_TParamCommandComment_getIndex {withVoided* %`Comment a', `Int' } -> `Int' #}
tParamCommandComment_getIndex :: Comment s -> Int -> IO Int
tParamCommandComment_getIndex c idx =
  clang_TParamCommandComment_getIndex c idx

-- CXString clang_VerbatimBlockLineComment_getText(CXComment Comment);
{# fun clang_VerbatimBlockLineComment_getText {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_VerbatimBlockLineComment_getText :: Comment s -> IO (ClangString ())
unsafe_VerbatimBlockLineComment_getText c =
  clang_VerbatimBlockLineComment_getText c >>= peek

verbatimBlockLineComment_getText :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
verbatimBlockLineComment_getText = registerClangString . unsafe_VerbatimBlockLineComment_getText

-- CXString clang_VerbatimLineComment_getText(CXComment Comment);
{# fun wrapped_clang_VerbatimLineComment_getText as clang_VerbatimLineComment_getText {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_VerbatimLineComment_getText :: Comment s -> IO (ClangString ())
unsafe_VerbatimLineComment_getText c =
  clang_VerbatimLineComment_getText c >>= peek

verbatimLineComment_getText :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
verbatimLineComment_getText = registerClangString . unsafe_VerbatimLineComment_getText

-- CXString clang_HTMLTagComment_getAsString(CXComment Comment);
{# fun wrapped_clang_HTMLTagComment_getAsString as clang_HTMLTagComment_getAsString {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_HTMLTagComment_getAsString :: Comment s -> IO (ClangString ())
unsafe_HTMLTagComment_getAsString c =
  clang_HTMLTagComment_getAsString c >>= peek

hTMLTagComment_getAsString :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
hTMLTagComment_getAsString = registerClangString . unsafe_HTMLTagComment_getAsString

-- CXString clang_FullComment_getAsHTML(CXComment Comment);
{# fun wrapped_clang_FullComment_getAsHTML as clang_FullComment_getAsHTML {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_FullComment_getAsHTML :: Comment s -> IO (ClangString ())
unsafe_FullComment_getAsHTML c =
  clang_FullComment_getAsHTML c >>= peek

fullComment_getAsHTML :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
fullComment_getAsHTML = registerClangString . unsafe_FullComment_getAsHTML

-- CXString clang_FullComment_getAsXML(CXComment Comment);
{# fun wrapped_clang_FullComment_getAsXML as clang_FullComment_getAsXML {withVoided* %`Comment a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_FullComment_getAsXML :: Comment s -> IO (ClangString ())
unsafe_FullComment_getAsXML c =
  clang_FullComment_getAsXML c >>= peek

fullComment_getAsXML :: ClangBase m => Comment s' -> ClangT s m (ClangString s)
fullComment_getAsXML = registerClangString . unsafe_FullComment_getAsXML

-- typedef enum CXTokenKind {
--   CXToken_Punctuation,
--   CXToken_Keyword,
--   CXToken_Identifier,
--   CXToken_Literal,
--   CXToken_Comment
-- } CXTokenKind;
#c
enum TokenKind
  { PunctuationToken = CXToken_Punctuation
  , KeywordToken     = CXToken_Keyword
  , IdentifierToken  = CXToken_Identifier
  , LiteralToken     = CXToken_Literal
  , CommentToken     = CXToken_Comment
 };
#endc
{# enum TokenKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- typedef struct {
--   unsigned int_data[4];
--   void *ptr_data;
-- } CXToken;
data Token s = Token !Int !Int !Int !Int !(Ptr ())
               deriving (Eq, Ord, Typeable)

instance ClangValue Token

instance Storable (Token s) where
    sizeOf _ = sizeOfCXToken
    {-# INLINE sizeOf #-}

    alignment _ = alignOfCXToken
    {-# INLINE alignment #-}

    peek p = do
      int_data <- {#get CXToken->int_data #} p >>= peekArray 4
      ptr_data <- {#get CXToken->ptr_data #} p
      return $! Token (fromIntegral (int_data !! 0)) (fromIntegral (int_data !! 1)) (fromIntegral (int_data !! 2)) (fromIntegral (int_data !! 3)) ptr_data
    {-# INLINE peek #-}

    poke p (Token i0 i1 i2 i3 ptr_data) = do
      intsArray <- mallocArray 4
      pokeArray intsArray (map fromIntegral [i0,i1,i2,i3])
      {#set CXToken->int_data #} p intsArray
      {#set CXToken->ptr_data #} p ptr_data
    {-# INLINE poke #-}

-- CXTokenKind clang_getTokenKind(CXToken);
{# fun clang_getTokenKind { withVoided* %`Token s' } -> `Int' #}
getTokenKind :: Token s -> IO TokenKind
getTokenKind t =
  clang_getTokenKind t  >>= return . toEnum

-- CXString clang_getTokenSpelling(CXTranslationUnit, CXToken);
{# fun wrapped_clang_getTokenSpelling as clang_getTokenSpelling { id `Ptr ()', withVoided* %`Token s' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getTokenSpelling :: TranslationUnit s -> Token s' -> IO (ClangString ())
unsafe_getTokenSpelling tu t =
   clang_getTokenSpelling (unTranslationUnit tu) t >>= peek

getTokenSpelling :: ClangBase m => TranslationUnit s' -> Token s'' -> ClangT s m (ClangString s)
getTokenSpelling = (registerClangString .) . unsafe_getTokenSpelling

-- CXSourceLocation clang_getTokenLocation(CXTranslationUnit,
--                                                        CXToken);
{# fun wrapped_clang_getTokenLocation as clang_getTokenLocation { id `Ptr ()', withVoided* %`Token a' } -> `Ptr (SourceLocation s)' castPtr #}
getTokenLocation :: Proxy s -> TranslationUnit t -> Token s' -> IO (SourceLocation s)
getTokenLocation _ tu t =
  clang_getTokenLocation (unTranslationUnit tu) t >>= peek

-- CXSourceRange clang_getTokenExtent(CXTranslationUnit, CXToken);
{# fun wrapped_clang_getTokenExtent as clang_getTokenExtent { id `Ptr ()', withVoided* %`Token a' } -> `Ptr (SourceRange s)' castPtr #}
getTokenExtent :: Proxy s -> TranslationUnit t -> Token s' -> IO (SourceRange s)
getTokenExtent _ tu t =
  clang_getTokenExtent (unTranslationUnit tu) t >>= peek

-- We deliberately don't export the constructor for UnsafeTokenList.
-- The only way to unwrap it is registerTokenList.
type TokenList s = DVS.Vector (Token s)
instance ClangValueList Token
data UnsafeTokenList = UnsafeTokenList !(Ptr ()) !Int

foreign import ccall unsafe "FFI_stub_ffi.h clang_disposeTokens" clang_disposeTokens :: Ptr () -> Ptr () -> CUInt -> IO ()

disposeTokens :: TranslationUnit s -> TokenList s' -> IO ()
disposeTokens tu tl =
 let (tPtr, n) = fromTokenList tl in
 clang_disposeTokens (unTranslationUnit tu) tPtr (fromIntegral n)

registerTokenList :: ClangBase m => TranslationUnit s' -> IO UnsafeTokenList
                  -> ClangT s m (TokenList s)
registerTokenList tu action = do
    (_, tokenList) <- clangAllocate (action >>= tokenListToVector) (disposeTokens tu)
    return tokenList
{-# INLINEABLE registerTokenList #-}

tokenListToVector :: Storable a => UnsafeTokenList -> IO (DVS.Vector a)
tokenListToVector (UnsafeTokenList ts n) = do
  fptr <- newForeignPtr_ (castPtr ts)
  return $ DVS.unsafeFromForeignPtr fptr 0 n
{-# INLINE tokenListToVector #-}

fromTokenList :: TokenList s -> (Ptr (), Int)
fromTokenList ts = let (p, _, _) = DVS.unsafeToForeignPtr ts in
                   (castPtr $ Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr p, DVS.length ts)

toTokenList :: (Ptr (), Int) -> UnsafeTokenList
toTokenList (ts, n) = UnsafeTokenList ts n

-- void clang_tokenize(CXTranslationUnit TU, CXSourceRange Range,
--                                    CXToken **Tokens, unsigned *NumTokens);
{# fun clang_tokenize { id `Ptr ()',withVoided* %`SourceRange a', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()'#}
unsafe_tokenize :: TranslationUnit s -> SourceRange s' -> IO UnsafeTokenList
unsafe_tokenize tu sr = do
  tokensPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr (Token ())))))
  numTokens <- clang_tokenize (unTranslationUnit tu) sr (castPtr tokensPtr)
  return (toTokenList (tokensPtr, fromIntegral numTokens))

tokenize :: ClangBase m => TranslationUnit s' -> SourceRange s'' -> ClangT s m (TokenList s)
tokenize tu = registerTokenList tu . unsafe_tokenize tu

-- TODO: test me
-- Note that registerCursorList can be used for the result of this
-- function because it just calls free() to dispose of the list.
--
-- void clang_annotateTokens(CXTranslationUnit TU,
--                                          CXToken *Tokens, unsigned NumTokens,
--                                          CXCursor *Cursors);
{# fun clang_annotateTokens { id `Ptr ()', id `Ptr ()', `Int', id `Ptr ()' } -> `()' id#}
unsafe_annotateTokens :: TranslationUnit s -> TokenList s' -> IO UnsafeCursorList
unsafe_annotateTokens tu tl =
  let (tPtr, numTokens) = fromTokenList tl in
  do
    cPtr <- mallocBytes ((sizeOf (undefined :: (Ptr (Cursor ())))) * numTokens)
    clang_annotateTokens (unTranslationUnit tu) tPtr numTokens (castPtr cPtr)
    return (toCursorList (cPtr, numTokens))

annotateTokens :: ClangBase m => TranslationUnit s' -> TokenList s'' -> ClangT s m (CursorList s)
annotateTokens = (registerCursorList .) . unsafe_annotateTokens

-- CXString clang_getCursorKindSpelling(enum CXCursorKind Kind);
{# fun wrapped_clang_getCursorKindSpelling as clang_getCursorKindSpelling { `CInt' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCursorKindSpelling :: CursorKind -> IO (ClangString ())
unsafe_getCursorKindSpelling ck = clang_getCursorKindSpelling (fromIntegral (fromEnum ck)) >>= peek

getCursorKindSpelling :: ClangBase m => CursorKind -> ClangT s m (ClangString s)
getCursorKindSpelling = registerClangString . unsafe_getCursorKindSpelling

-- void clang_enableStackTraces(void);
foreign import ccall unsafe "clang-c/Index.h clang_enableStackTraces" enableStackTraces :: IO ()

-- typedef void *CXCompletionString;

-- | A semantic string that describes a code completion result.
--
-- A 'CompletionString' describes the formatting of a code completion
-- result as a single \"template\" of text that should be inserted into the
-- source buffer when a particular code completion result is selected.
-- Each semantic string is made up of some number of \"chunks\", each of which
-- contains some text along with a description of what that text means.
--
-- See 'ChunkKind' for more details about the role each kind of chunk plays.
newtype CompletionString s = CompletionString (Ptr ())
                             deriving (Eq, Ord, Typeable)

instance ClangValue CompletionString

-- typedef struct {
--   enum CXCursorKind CursorKind;
--   CXCompletionString CompletionString;
-- } CXCompletionResult;
data CompletionResult s = CompletionResult !CursorKind !(CompletionString s)
                          deriving (Eq, Ord, Typeable)

instance ClangValue CompletionResult

-- enum CXCompletionChunkKind {
--   CXCompletionChunk_Optional,
--   CXCompletionChunk_TypedText,
--   CXCompletionChunk_Text,
--   CXCompletionChunk_Placeholder,
--   CXCompletionChunk_Informative,
--   CXCompletionChunk_CurrentParameter,
--   CXCompletionChunk_LeftParen,
--   CXCompletionChunk_RightParen,
--   CXCompletionChunk_LeftBracket,
--   CXCompletionChunk_RightBracket,
--   CXCompletionChunk_LeftBrace,
--   CXCompletionChunk_RightBrace,
--   CXCompletionChunk_LeftAngle,
--   CXCompletionChunk_RightAngle,
--   CXCompletionChunk_Comma,
--   CXCompletionChunk_ResultType,
--   CXCompletionChunk_Colon,
--   CXCompletionChunk_SemiColon,
--   CXCompletionChunk_Equal,
--   CXCompletionChunk_HorizontalSpace,
--   CXCompletionChunk_VerticalSpace
-- };

-- | Describes a single piece of text within a code completion string.
--
-- * 'OptionalChunkKind': A code completion string that describes "optional" text that
--   could be a part of the template (but is not required).
--   This is the only kind of chunk that has a code completion
--   string for its representation. The code completion string describes an
--   describes an additional part of the template that is completely optional.
--   For example, optional chunks can be used to describe the placeholders for
--   arguments that match up with defaulted function parameters.
--
-- * 'TypedTextChunkKind': Text that a user would be expected to type to get this
--   code completion result.
--   There will be exactly one \"typed text\" chunk in a semantic string, which
--   will typically provide the spelling of a keyword or the name of a
--   declaration that could be used at the current code point. Clients are
--   expected to filter the code completion results based on the text in this
--   chunk.
--
-- * 'TextChunkKind': Text that should be inserted as part of a code completion result.
--   A \"text\" chunk represents text that is part of the template to be
--   inserted into user code should this particular code completion result
--   be selected.
--
-- * 'PlaceholderChunkKind': Placeholder text that should be replaced by the user.
--   A \"placeholder\" chunk marks a place where the user should insert text
--   into the code completion template. For example, placeholders might mark
--   the function parameters for a function declaration, to indicate that the
--   user should provide arguments for each of those parameters. The actual
--   text in a placeholder is a suggestion for the text to display before
--   the user replaces the placeholder with real code.
--
-- * 'InformativeChunkKind': Informative text that should be displayed but never inserted as
--   part of the template.
--   An \"informative\" chunk contains annotations that can be displayed to
--   help the user decide whether a particular code completion result is the
--   right option, but which is not part of the actual template to be inserted
--   by code completion.
--
-- * 'CurrentParameterChunkKind': Text that describes the current parameter
--   when code completion is referring to a function call, message send, or
--   template specialization.
--   A \"current parameter\" chunk occurs when code completion is providing
--   information about a parameter corresponding to the argument at the
--   code completion point. For example, given a function \"int add(int x, int y);\"
--   and the source code \"add(\", where the code completion point is after the
--   \"(\", the code completion string will contain a \"current parameter\" chunk
--   for \"int x\", indicating that the current argument will initialize that
--   parameter. After typing further, to \"add(17\", (where the code completion
--   point is after the \",\"), the code completion string will contain a
--   \"current parameter\" chunk to \"int y\".
--
-- * 'LeftParenChunkKind': A left parenthesis (\'(\'), used to initiate a function call or
--   signal the beginning of a function parameter list.
--
-- * 'RightParenChunkKind': A right parenthesis (\')\'), used to finish a function call or
--   signal the end of a function parameter list.
--
-- * 'LeftBracketChunkKind': A left bracket (\'[\').
--
-- * 'RightBracketChunkKind': A right bracket (\']\').
--
-- * 'LeftBraceChunkKind': A left brace (\'{\').
--
-- * 'RightBraceChunkKind': A right brace (\'}\').
--
-- * 'LeftAngleChunkKind': A left angle bracket (\'<\').
--
-- * 'RightAngleChunkKind': A right angle bracket (\'>\').
--
-- * 'CommaChunkKind': A comma separator (\',\').
--
-- * 'ResultTypeChunkKind': Text that specifies the result type of a given result.
--   This special kind of informative chunk is not meant to be inserted into
--   the text buffer. Rather, it is meant to illustrate the type that an
--   expression using the given completion string would have.
--
-- * 'ColonChunkKind': A colon (\':\').
--
-- * 'SemiColonChunkKind': A semicolon (\';\').
--
-- * 'EqualChunkKind': An \'=\' sign.
--
-- * 'HorizontalSpaceChunkKind': Horizontal space (\' \').
--
-- * 'VerticalSpaceChunkKind': Vertical space (\'\\n\'), after which it is generally
--   a good idea to perform indentation.
#c
enum ChunkKind
  { OptionalChunkKind         = CXCompletionChunk_Optional
  , TypedTextChunkKind        = CXCompletionChunk_TypedText
  , TextChunkKind             = CXCompletionChunk_Text
  , PlaceholderChunkKind      = CXCompletionChunk_Placeholder
  , InformativeChunkKind      = CXCompletionChunk_Informative
  , CurrentParameterChunkKind = CXCompletionChunk_CurrentParameter
  , LeftParenChunkKind        = CXCompletionChunk_LeftParen
  , RightParenChunkKind       = CXCompletionChunk_RightParen
  , LeftBracketChunkKind      = CXCompletionChunk_LeftBracket
  , RightBracketChunkKind     = CXCompletionChunk_RightBracket
  , LeftBraceChunkKind        = CXCompletionChunk_LeftBrace
  , RightBraceChunkKind       = CXCompletionChunk_RightBrace
  , LeftAngleChunkKind        = CXCompletionChunk_LeftAngle
  , RightAngleChunkKind       = CXCompletionChunk_RightAngle
  , CommaChunkKind            = CXCompletionChunk_Comma
  , ResultTypeChunkKind       = CXCompletionChunk_ResultType
  , ColonChunkKind            = CXCompletionChunk_Colon
  , SemiColonChunkKind        = CXCompletionChunk_SemiColon
  , EqualChunkKind            = CXCompletionChunk_Equal
  , HorizontalSpaceChunkKind  = CXCompletionChunk_HorizontalSpace
  , VerticalSpaceChunkKind    = CXCompletionChunk_VerticalSpace
 };
#endc
{# enum ChunkKind {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

-- enum CXCompletionChunkKind
-- clang_getCompletionChunkKind(CXCompletionString completion_string,
--                              unsigned chunk_number);
{# fun clang_getCompletionChunkKind { id `Ptr ()', `Int'} -> `Int' #}
getCompletionChunkKind :: CompletionString s -> Int -> IO ChunkKind
getCompletionChunkKind (CompletionString ptr) i = clang_getCompletionChunkKind ptr i >>= return . toEnum

-- CXString
-- clang_getCompletionChunkText(CXCompletionString completion_string,
--                              unsigned chunk_number);
{# fun wrapped_clang_getCompletionChunkText as clang_getCompletionChunkText { id `Ptr ()' , `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCompletionChunkText :: CompletionString s -> Int -> IO (ClangString ())
unsafe_getCompletionChunkText (CompletionString ptr) i = clang_getCompletionChunkText ptr i >>= peek

getCompletionChunkText :: ClangBase m => CompletionString s' -> Int -> ClangT s m (ClangString s)
getCompletionChunkText = (registerClangString .) . unsafe_getCompletionChunkText

-- CXCompletionString
-- clang_getCompletionChunkCompletionString(CXCompletionString completion_string,
--                                          unsigned chunk_number);
{# fun clang_getCompletionChunkCompletionString { id `Ptr ()' , `Int' } -> `Ptr ()' id #}
getCompletionChunkCompletionString :: CompletionString s -> Int -> IO (CompletionString s')
getCompletionChunkCompletionString (CompletionString ptr) i =
  clang_getCompletionChunkCompletionString ptr i >>= return . CompletionString

-- unsigned
-- clang_getNumCompletionChunks(CXCompletionString completion_string);
{# fun clang_getNumCompletionChunks { id `Ptr ()' } -> `Int' #}
getNumCompletionChunks :: CompletionString s -> IO Int
getNumCompletionChunks (CompletionString ptr) = clang_getNumCompletionChunks ptr

-- unsigned
-- clang_getCompletionPriority(CXCompletionString completion_string);
{# fun clang_getCompletionPriority { id `Ptr ()' } -> `Int' #}
getCompletionPriority :: CompletionString s -> IO Int
getCompletionPriority (CompletionString ptr) = clang_getCompletionPriority ptr

-- enum CXAvailabilityKind
-- clang_getCompletionAvailability(CXCompletionString completion_string);
{# fun clang_getCompletionAvailability { id `Ptr ()' } -> `Int' #}
getCompletionAvailability :: CompletionString s ->  IO AvailabilityKind
getCompletionAvailability (CompletionString ptr) = clang_getCompletionAvailability ptr >>= return . toEnum

-- unsigned clang_getCompletionNumAnnotations(CXCompletionString completion_string);
{# fun clang_getCompletionNumAnnotations { id `Ptr ()' } -> `Int' #}
getCompletionNumAnnotations :: CompletionString s -> IO Int
getCompletionNumAnnotations (CompletionString ptr) = clang_getCompletionNumAnnotations ptr

-- CXString clang_getCompletionAnnotation(CXCompletionString completion_string,
--                                        unsigned annotation_number);
{# fun wrapped_clang_getCompletionAnnotation as clang_getCompletionAnnotation { id `Ptr ()' , `Int' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCompletionAnnotation :: CompletionString s -> Int -> IO (ClangString ())
unsafe_getCompletionAnnotation (CompletionString ptr) i = clang_getCompletionAnnotation ptr i >>= peek

getCompletionAnnotation :: ClangBase m => CompletionString s' -> Int -> ClangT s m (ClangString s)
getCompletionAnnotation = (registerClangString .) . unsafe_getCompletionAnnotation

-- CXString clang_getCompletionParent(CXCompletionString completion_string,
--                                    enum CXCursorKind *kind);
{# fun wrapped_clang_getCompletionParent as clang_getCompletionParent { id `Ptr ()' , `CInt' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCompletionParent :: CompletionString s  -> IO (ClangString ())
unsafe_getCompletionParent (CompletionString ptr) = clang_getCompletionParent ptr 0 >>= peek

getCompletionParent :: ClangBase m => CompletionString s' -> ClangT s m (ClangString s)
getCompletionParent = registerClangString . unsafe_getCompletionParent

-- CXString clang_getCompletionBriefComment(CXCompletionString completion_string);
{# fun wrapped_clang_getCompletionBriefComment as clang_getCompletionBriefComment { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCompletionBriefComment :: CompletionString s  -> IO (ClangString ())
unsafe_getCompletionBriefComment (CompletionString ptr) = clang_getCompletionBriefComment ptr >>= peek

getCompletionBriefComment :: ClangBase m => CompletionString s' -> ClangT s m (ClangString s)
getCompletionBriefComment = registerClangString . unsafe_getCompletionBriefComment

-- CXCompletionString clang_getCursorCompletionString(CXCursor cursor);
{# fun clang_getCursorCompletionString {withVoided* %`Cursor a' } -> `Ptr (ClangString ())' castPtr #}
unsafe_getCursorCompletionString :: Cursor s'  -> IO (ClangString ())
unsafe_getCursorCompletionString c =
  clang_getCursorCompletionString c >>= peek

getCursorCompletionString :: ClangBase m => Cursor s' -> ClangT s m (CompletionString s)
getCursorCompletionString c =
  unsafeCoerce <$> (liftIO $ unsafe_getCursorCompletionString c)

-- enum CXCodeComplete_Flags {
--   CXCodeComplete_IncludeMacros = 0x01,
--   CXCodeComplete_IncludeCodePatterns = 0x02,
--   CXCodeComplete_IncludeBriefComments = 0x04
-- };

-- | Flags that can be used to modify the behavior of 'codeCompleteAt'.
--
-- * 'IncludeMacros': Whether to include macros within the set of code
--   completions returned.
--
-- * 'IncludeCodePatterns': Whether to include code patterns for language constructs
--   within the set of code completions, e.g., 'for' loops.
--
-- * 'IncludeBriefComments': Whether to include brief documentation within the set of code
--   completions returned.
#c
enum CodeCompleteFlags
  { IncludeMacros        = CXCodeComplete_IncludeMacros
  , IncludeCodePatterns  = CXCodeComplete_IncludeCodePatterns
  , IncludeBriefComments = CXCodeComplete_IncludeBriefComments
 };
#endc
{# enum CodeCompleteFlags {} deriving  (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags CodeCompleteFlags where
  toBit IncludeMacros        = 0x01
  toBit IncludeCodePatterns  = 0x02
  toBit IncludeBriefComments = 0x04

-- unsigned clang_defaultCodeCompleteOptions(void);
{# fun clang_defaultCodeCompleteOptions as defaultCodeCompleteOptions {} -> `Int' #}

-- typedef struct {
--   CXCompletionResult *Results;
--   unsigned NumResults;
-- } CXCodeCompleteResults;

-- | The results of code completion.
newtype CodeCompleteResults s = CodeCompleteResults { unCodeCompleteResults :: Ptr () }
                                deriving (Eq, Ord, Typeable)

instance ClangValue CodeCompleteResults

-- void clang_disposeCodeCompleteResults(CXCodeCompleteResults *Results);
{# fun clang_disposeCodeCompleteResults { id `Ptr ()' } -> `()' #}
disposeCodeCompleteResults :: CodeCompleteResults s -> IO ()
disposeCodeCompleteResults rs = clang_disposeCodeCompleteResults (unCodeCompleteResults rs)

registerCodeCompleteResults :: ClangBase m => IO (CodeCompleteResults ())
                            -> ClangT s m (CodeCompleteResults s)
registerCodeCompleteResults action = do
  (_, ccrs) <- clangAllocate (action >>= return . unsafeCoerce)
                             disposeCodeCompleteResults
  return ccrs
{-# INLINEABLE registerCodeCompleteResults #-}

-- CXCodeCompleteResults *clang_codeCompleteAt(CXTranslationUnit TU,
--                                             const char *complete_filename,
--                                             unsigned complete_line,
--                                             unsigned complete_column,
--                                             struct CXUnsavedFile *unsaved_files,
--                                             unsigned num_unsaved_files,
--                                             unsigned options);
{# fun clang_codeCompleteAt { id `Ptr ()', `CString', `Int', `Int', id `Ptr ()', `Int', `Int' } -> `Ptr ()' id #}
unsafe_codeCompleteAt :: TranslationUnit s -> String -> Int -> Int -> Ptr CUnsavedFile -> Int -> Int -> IO (CodeCompleteResults ())
unsafe_codeCompleteAt tu s i1 i2 ufs nufs i3 =
  withCString s (\sPtr -> clang_codeCompleteAt (unTranslationUnit tu) sPtr i1 i2 (castPtr ufs) nufs i3 >>= return . CodeCompleteResults)

codeCompleteAt :: ClangBase m => TranslationUnit s' -> String -> Int -> Int
               -> DV.Vector UnsavedFile -> Int -> ClangT s m (CodeCompleteResults s)
codeCompleteAt tu f l c ufs os =
  registerCodeCompleteResults $
    withUnsavedFiles ufs $ \ufsPtr ufsLen ->
      unsafe_codeCompleteAt tu f l c ufsPtr ufsLen os

-- This function, along with codeCompleteGetResult, exist to allow iteration over
-- the completion strings at the Haskell level. They're (obviously) not real
-- libclang functions.
{# fun codeCompleteGetNumResults as codeCompleteGetNumResults' { id `Ptr ()' } -> `Int' #}
codeCompleteGetNumResults :: CodeCompleteResults s -> IO Int
codeCompleteGetNumResults rs = codeCompleteGetNumResults' (unCodeCompleteResults rs)

-- We don't need to register CompletionStrings; they're always owned by another object.
-- They still need to be scoped, though.
{# fun codeCompleteGetResult as codeCompleteGetResult' { id `Ptr ()', `CInt', id `Ptr (Ptr ())' } -> `CInt' id #}
unsafe_codeCompleteGetResult :: CodeCompleteResults s -> Int -> IO (CompletionString (), CursorKind)
unsafe_codeCompleteGetResult rs idx = do
 sPtr <- mallocBytes (sizeOf (undefined :: (Ptr (Ptr ()))))
 kind <- codeCompleteGetResult' (unCodeCompleteResults rs) (fromIntegral idx) (castPtr sPtr)
 return ((CompletionString sPtr), toEnum (fromIntegral kind))

codeCompleteGetResult :: ClangBase m => CodeCompleteResults s' -> Int
                      -> ClangT s m (CompletionString s, CursorKind)
codeCompleteGetResult rs n = do
  (string, kind) <- liftIO $ unsafe_codeCompleteGetResult rs n
  return (unsafeCoerce string, kind)

-- void clang_sortCodeCompletionResults(CXCompletionResult *Results,
--                                      unsigned NumResults);
{# fun clang_sortCodeCompletionResults { id `Ptr ()', `Int' } -> `()' #}
sortCodeCompletionResults :: CodeCompleteResults s -> IO ()
sortCodeCompletionResults rs =
  let results = unCodeCompleteResults rs in
  do
    rPtr <- peek (results `plusPtr` offsetCXCodeCompleteResultsResults)
    numRs <- peek (results `plusPtr` offsetCXCodeCompleteResultsNumResults)
    clang_sortCodeCompletionResults rPtr numRs

-- unsigned clang_codeCompleteGetNumDiagnostics(CXCodeCompleteResults *Results);
{# fun clang_codeCompleteGetNumDiagnostics { id `Ptr ()' } -> `Int' #}
codeCompleteGetNumDiagnostics :: CodeCompleteResults s -> IO Int
codeCompleteGetNumDiagnostics rs = clang_codeCompleteGetNumDiagnostics (unCodeCompleteResults rs)

-- CXDiagnostic clang_codeCompleteGetDiagnostic(CXCodeCompleteResults *Results,
--                                              unsigned Index);
{# fun clang_codeCompleteGetDiagnostic { id `Ptr ()' , `Int' } -> `Ptr ()' id #}
unsafe_codeCompleteGetDiagnostic :: CodeCompleteResults s -> Int -> IO (Diagnostic ())
unsafe_codeCompleteGetDiagnostic rs idx =
   clang_codeCompleteGetDiagnostic (unCodeCompleteResults rs) idx >>= return . mkDiagnostic

codeCompleteGetDiagnostic :: ClangBase m => CodeCompleteResults s' -> Int
                          -> ClangT s m (Diagnostic s)
codeCompleteGetDiagnostic = (registerDiagnostic .) . unsafe_codeCompleteGetDiagnostic

-- enum CXCompletionContext {
--   CXCompletionContext_Unexposed = 0,
--   CXCompletionContext_AnyType = 1 << 0,
--   CXCompletionContext_AnyValue = 1 << 1,
--   CXCompletionContext_ObjCObjectValue = 1 << 2,
--   CXCompletionContext_ObjCSelectorValue = 1 << 3,
--   CXCompletionContext_CXXClassTypeValue = 1 << 4,
--   CXCompletionContext_DotMemberAccess = 1 << 5,
--   CXCompletionContext_ArrowMemberAccess = 1 << 6,
--   CXCompletionContext_ObjCPropertyAccess = 1 << 7,
--   CXCompletionContext_EnumTag = 1 << 8,
--   CXCompletionContext_UnionTag = 1 << 9,
--   CXCompletionContext_StructTag = 1 << 10,
--   CXCompletionContext_ClassTag = 1 << 11,
--   CXCompletionContext_Namespace = 1 << 12,
--   CXCompletionContext_NestedNameSpecifier = 1 << 13,
--   CXCompletionContext_ObjCInterface = 1 << 14,
--   CXCompletionContext_ObjCProtocol = 1 << 15,
--   CXCompletionContext_ObjCCategory = 1 << 16,
--   CXCompletionContext_ObjCInstanceMessage = 1 << 17,
--   CXCompletionContext_ObjCClassMessage = 1 << 18,
--   CXCompletionContext_ObjCSelectorName = 1 << 19,
--   CXCompletionContext_MacroName = 1 << 20,
--   CXCompletionContext_NaturalLanguage = 1 << 21,
--   CXCompletionContext_Unknown = ((1 << 22) - 1) -- Set all
                            --   contexts... Not a real value.
-- };

-- | Contexts under which completion may occur. Multiple contexts may be
-- present at the same time.
--
-- * 'UnexposedCompletionContext': The context for completions is unexposed,
--   as only Clang results should be included.
--
-- * 'AnyTypeCompletionContext': Completions for any possible type should be
--   included in the results.
--
-- * 'AnyValueCompletionContext': Completions for any possible value (variables,
--   function calls, etc.) should be included in the results.
--
-- * 'ObjCObjectValueCompletionContext': Completions for values that resolve to
--   an Objective-C object should be included in the results.
--
-- * 'ObjCSelectorValueCompletionContext': Completions for values that resolve
--   to an Objective-C selector should be included in the results.
--
-- * 'CXXClassTypeValueCompletionContext': Completions for values that resolve
--   to a C++ class type should be included in the results.
--
-- * 'DotMemberAccessCompletionContext': Completions for fields of the member
--   being accessed using the dot operator should be included in the results.
--
-- * 'ArrowMemberAccessCompletionContext': Completions for fields of the member
--   being accessed using the arrow operator should be included in the results.
--
-- * 'ObjCPropertyAccessCompletionContext': Completions for properties of the
--   Objective-C object being accessed using the dot operator should be included in the results.
--
-- * 'EnumTagCompletionContext': Completions for enum tags should be included in the results.
--
-- * 'UnionTagCompletionContext': Completions for union tags should be included in the results.
--
-- * 'StructTagCompletionContext': Completions for struct tags should be included in the
--   results.
--
-- * 'ClassTagCompletionContext': Completions for C++ class names should be included in the
--   results.
--
-- * 'NamespaceCompletionContext': Completions for C++ namespaces and namespace aliases should
--   be included in the results.
--
-- * 'NestedNameSpecifierCompletionContext': Completions for C++ nested name specifiers should
--   be included in the results.
--
-- * 'ObjCInterfaceCompletionContext': Completions for Objective-C interfaces (classes) should
--   be included in the results.
--
-- * 'ObjCProtocolCompletionContext': Completions for Objective-C protocols should be included
--   in the results.
--
-- * 'ObjCCategoryCompletionContext': Completions for Objective-C categories should be included
--   in the results.
--
-- * 'ObjCInstanceMessageCompletionContext': Completions for Objective-C instance messages
--   should be included in the results.
--
-- * 'ObjCClassMessageCompletionContext': Completions for Objective-C class messages should be
--   included in the results.
--
-- * 'ObjCSelectorNameCompletionContext': Completions for Objective-C selector names should be
--   included in the results.
--
-- * 'MacroNameCompletionContext': Completions for preprocessor macro names should be included
--   in the results.
--
-- * 'NaturalLanguageCompletionContext': Natural language completions should be included in the
--   results.
#c
enum CompletionContext
  { UnexposedCompletionContext            = CXCompletionContext_Unexposed
  , AnyTypeCompletionContext              = CXCompletionContext_AnyType
  , AnyValueCompletionContext             = CXCompletionContext_AnyValue
  , ObjCObjectValueCompletionContext      = CXCompletionContext_ObjCObjectValue
  , ObjCSelectorValueCompletionContext    = CXCompletionContext_ObjCSelectorValue
  , CXXClassTypeValueCompletionContext    = CXCompletionContext_CXXClassTypeValue
  , DotMemberAccessCompletionContext      = CXCompletionContext_DotMemberAccess
  , ArrowMemberAccessCompletionContext    = CXCompletionContext_ArrowMemberAccess
  , ObjCPropertyAccessCompletionContext   = CXCompletionContext_ObjCPropertyAccess
  , EnumTagCompletionContext              = CXCompletionContext_EnumTag
  , UnionTagCompletionContext             = CXCompletionContext_UnionTag
  , StructTagCompletionContext            = CXCompletionContext_StructTag
  , ClassTagCompletionContext             = CXCompletionContext_ClassTag
  , NamespaceCompletionContext            = CXCompletionContext_Namespace
  , NestedNameSpecifierCompletionContext  = CXCompletionContext_NestedNameSpecifier
  , ObjCInterfaceCompletionContext        = CXCompletionContext_ObjCInterface
  , ObjCProtocolCompletionContext         = CXCompletionContext_ObjCProtocol
  , ObjCCategoryCompletionContext         = CXCompletionContext_ObjCCategory
  , ObjCInstanceMessageCompletionContext  = CXCompletionContext_ObjCInstanceMessage
  , ObjCClassMessageCompletionContext     = CXCompletionContext_ObjCClassMessage
  , ObjCSelectorNameCompletionContext     = CXCompletionContext_ObjCSelectorName
  , MacroNameCompletionContext            = CXCompletionContext_MacroName
  , NaturalLanguageCompletionContext      = CXCompletionContext_NaturalLanguage
 };
#endc
{# enum CompletionContext {} deriving (Bounded, Eq, Ord, Read, Show, Typeable) #}

instance BitFlags CompletionContext where
  type FlagInt CompletionContext             = Int64
  toBit UnexposedCompletionContext           = 0x0
  toBit AnyTypeCompletionContext             = 0x1
  toBit AnyValueCompletionContext            = 0x2
  toBit ObjCObjectValueCompletionContext     = 0x4
  toBit ObjCSelectorValueCompletionContext   = 0x8
  toBit CXXClassTypeValueCompletionContext   = 0x10
  toBit DotMemberAccessCompletionContext     = 0x20
  toBit ArrowMemberAccessCompletionContext   = 0x40
  toBit ObjCPropertyAccessCompletionContext  = 0x80
  toBit EnumTagCompletionContext             = 0x100
  toBit UnionTagCompletionContext            = 0x200
  toBit StructTagCompletionContext           = 0x400
  toBit ClassTagCompletionContext            = 0x800
  toBit NamespaceCompletionContext           = 0x1000
  toBit NestedNameSpecifierCompletionContext = 0x2000
  toBit ObjCInterfaceCompletionContext       = 0x4000
  toBit ObjCProtocolCompletionContext        = 0x8000
  toBit ObjCCategoryCompletionContext        = 0x10000
  toBit ObjCInstanceMessageCompletionContext = 0x20000
  toBit ObjCClassMessageCompletionContext    = 0x40000
  toBit ObjCSelectorNameCompletionContext    = 0x80000
  toBit MacroNameCompletionContext           = 0x100000
  toBit NaturalLanguageCompletionContext     = 0x200000

-- unsigned long long clang_codeCompleteGetContexts(CXCodeCompleteResults *Results);
{# fun clang_codeCompleteGetContexts { id `Ptr ()' } -> `Int64' #}
codeCompleteGetContexts :: CodeCompleteResults s -> IO Int64
codeCompleteGetContexts rs = clang_codeCompleteGetContexts (unCodeCompleteResults rs)

-- enum CXCursorKind clang_codeCompleteGetContainerKind(CXCodeCompleteResults *Results,
--                                                      unsigned *IsIncomplete);
{# fun clang_codeCompleteGetContainerKind { id `Ptr ()', id `Ptr CUInt' } -> `Int' #}
codeCompleteGetContainerKind :: CodeCompleteResults s -> IO (CursorKind, Bool)
codeCompleteGetContainerKind rs =
  alloca (\(iPtr :: (Ptr CUInt)) -> do
     k <- clang_codeCompleteGetContainerKind (unCodeCompleteResults rs) iPtr
     bool <- peek iPtr
     return (toEnum k, toBool ((fromIntegral bool) :: Int)))

-- CXString clang_codeCompleteGetContainerUSR(CXCodeCompleteResults *Results);
{# fun clang_codeCompleteGetContainerUSR { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_codeCompleteGetContainerUSR :: CodeCompleteResults s -> IO (ClangString ())
unsafe_codeCompleteGetContainerUSR rs = clang_codeCompleteGetContainerUSR (unCodeCompleteResults rs) >>= peek

codeCompleteGetContainerUSR :: ClangBase m => CodeCompleteResults s' -> ClangT s m (ClangString s)
codeCompleteGetContainerUSR = registerClangString . unsafe_codeCompleteGetContainerUSR

-- CXString clang_codeCompleteGetObjCSelector(CXCodeCompleteResults *Results);
{# fun clang_codeCompleteGetObjCSelector { id `Ptr ()' } -> `Ptr (ClangString ())' castPtr #}
unsafe_codeCompleteGetObjCSelector :: CodeCompleteResults s -> IO (ClangString ())
unsafe_codeCompleteGetObjCSelector rs = clang_codeCompleteGetObjCSelector (unCodeCompleteResults rs) >>= peek

codeCompleteGetObjCSelector :: ClangBase m => CodeCompleteResults s' -> ClangT s m (ClangString s)
codeCompleteGetObjCSelector = registerClangString . unsafe_codeCompleteGetObjCSelector

-- CXString clang_getClangVersion();
{# fun wrapped_clang_getClangVersion as clang_getClangVersion {} -> `Ptr (ClangString ())' castPtr #}
unsafe_getClangVersion :: IO (ClangString ())
unsafe_getClangVersion = clang_getClangVersion >>= peek

getClangVersion :: ClangBase m => ClangT s m (ClangString s)
getClangVersion = registerClangString $ unsafe_getClangVersion

-- -- void clang_toggleCrashRecovery(unsigned isEnabled);
{# fun clang_toggleCrashRecovery as toggleCrashRecovery' { `Int' } -> `()' #}
toggleCrashRecovery :: Bool -> IO ()
toggleCrashRecovery b = toggleCrashRecovery' (fromBool b)

data Inclusion s = Inclusion !(File s) !(SourceLocation s) !Bool
                   deriving (Eq, Ord, Typeable)

instance ClangValue Inclusion

instance Storable (Inclusion s) where
    sizeOf _ = sizeOfInclusion
    {-# INLINE sizeOf #-}

    alignment _ = alignOfInclusion
    {-# INLINE alignment #-}

    peek p = do
      let fromCUChar = fromIntegral :: Num b => CUChar -> b
      file <- File <$> peekByteOff p offsetInclusionInclusion
      sl <- peekByteOff p offsetInclusionLocation
      isDirect :: Int <- fromCUChar <$> peekByteOff p offsetInclusionIsDirect
      return $! Inclusion file sl (if isDirect == 0 then False else True)
    {-# INLINE peek #-}

    poke p (Inclusion (File file) sl isDirect) = do
      let toCUChar = fromIntegral :: Integral a => a -> CUChar
      pokeByteOff p offsetInclusionInclusion file
      pokeByteOff p offsetInclusionLocation sl
      pokeByteOff p offsetInclusionIsDirect
           (toCUChar $ if isDirect then 1 :: Int else 0)
    {-# INLINE poke #-}

type InclusionList s = DVS.Vector (Inclusion s)
instance ClangValueList Inclusion
data UnsafeInclusionList = UnsafeInclusionList !(Ptr ()) !Int

-- void freeInclusions(struct Inclusion* inclusions);
{# fun freeInclusionList as freeInclusionList' { id `Ptr ()' } -> `()' #}
freeInclusions :: InclusionList s -> IO ()
freeInclusions is =
  let (isPtr, n) = fromInclusionList is in
  freeInclusionList' isPtr

registerInclusionList :: ClangBase m => IO UnsafeInclusionList -> ClangT s m (InclusionList s)
registerInclusionList action = do
    (_, inclusionList) <- clangAllocate (action >>= mkSafe) freeInclusions
    return inclusionList
  where
    mkSafe (UnsafeInclusionList is n) = do
      fptr <- newForeignPtr_ (castPtr is)
      return $ DVS.unsafeFromForeignPtr fptr 0 n
{-# INLINEABLE registerInclusionList #-}

fromInclusionList :: InclusionList s -> (Ptr (), Int)
fromInclusionList is = let (p, _, _) = DVS.unsafeToForeignPtr is in
                       (castPtr $ Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr p, DVS.length is)

toInclusionList :: (Ptr (), Int) -> UnsafeInclusionList
toInclusionList (is, n) = UnsafeInclusionList is n

#c
typedef Inclusion** PtrPtrInclusion;
#endc

-- A more efficient alternative to clang_getInclusions.
-- void getInclusions(CXTranslationUnit tu, struct Inclusion** inclusionsOut, unsigned* countOut)
{# fun getInclusions as getInclusions' { id `Ptr ()', id `Ptr (Ptr ())', alloca- `CUInt' peek* } -> `()' #}
unsafe_getInclusions :: TranslationUnit s -> IO UnsafeInclusionList
unsafe_getInclusions tu = do
  iPtrPtr <- mallocBytes {#sizeof PtrPtrInclusion #}
  n <- getInclusions' (unTranslationUnit tu) (castPtr iPtrPtr)
  firstInclusion <- peek (castPtr iPtrPtr :: Ptr (Ptr ()))
  free iPtrPtr
  return (toInclusionList (firstInclusion, fromIntegral n))


getInclusions :: ClangBase m => TranslationUnit s' -> ClangT s m (InclusionList s)
getInclusions = registerInclusionList . unsafe_getInclusions

-- typedef void* CXRemapping;
newtype Remapping s = Remapping { unRemapping :: Ptr () }
                      deriving (Eq, Ord, Typeable)

instance ClangValue Remapping

mkRemapping :: Ptr () -> Remapping ()
mkRemapping = Remapping

-- void clang_remap_dispose(CXRemapping);
{# fun clang_remap_dispose { id `Ptr ()' } -> `()' #}
remap_dispose :: Remapping s -> IO ()
remap_dispose d = clang_remap_dispose (unRemapping d)

registerRemapping :: ClangBase m => IO (Remapping ()) -> ClangT s m (Remapping s)
registerRemapping action = do
  (_, idx) <- clangAllocate (action >>= return . unsafeCoerce)
                            (\i -> remap_dispose i)
  return idx
{-# INLINEABLE registerRemapping #-}

-- %dis remapping i = <unRemapping/mkRemapping> (ptr i)

maybeRemapping :: Remapping s' -> Maybe (Remapping s)
maybeRemapping (Remapping p) | p == nullPtr = Nothing
maybeRemapping f                            = Just (unsafeCoerce f)

unMaybeRemapping :: Maybe (Remapping s') -> Remapping s
unMaybeRemapping (Just f) = unsafeCoerce f
unMaybeRemapping Nothing  = Remapping nullPtr

-- CXRemapping clang_getRemappings(const char *path);
{# fun clang_getRemappings { `CString' } -> `Ptr ()' id #}
unsafe_getRemappings :: FilePath -> IO (Maybe (Remapping ()))
unsafe_getRemappings fp =
  withCString fp (\sPtr -> clang_getRemappings sPtr >>= return . maybeRemapping . mkRemapping )

getRemappings :: ClangBase m => FilePath -> ClangT s m (Maybe (Remapping s))
getRemappings path = do
  mRemappings <- liftIO $ unsafe_getRemappings path
  case mRemappings of
    Just remappings -> Just <$> registerRemapping (return remappings)
    Nothing         -> return Nothing

-- CXRemapping clang_getRemappingsFromFileList(const char **filePaths, unsigned numFiles);
{# fun clang_getRemappingsFromFileList { id `Ptr CString' , `Int' } -> `Ptr ()' id #}
unsafe_getRemappingsFromFileList :: Ptr CString -> Int -> IO (Maybe (Remapping ()))
unsafe_getRemappingsFromFileList paths numPaths =
  clang_getRemappingsFromFileList paths numPaths >>= return . maybeRemapping . mkRemapping

getRemappingsFromFileList :: ClangBase m => [FilePath] -> ClangT s m (Maybe (Remapping s))
getRemappingsFromFileList paths = do
  mRemappings <- liftIO $ withStringList paths unsafe_getRemappingsFromFileList
  case mRemappings of
    Just remappings -> Just <$> registerRemapping (return remappings)
    Nothing         -> return Nothing

-- unsigned clang_remap_getNumFiles(CXRemapping);
{# fun clang_remap_getNumFiles { id `Ptr ()' } -> `Int' #}
remap_getNumFiles :: Remapping s -> IO Int
remap_getNumFiles remaps =
  clang_remap_getNumFiles (unRemapping remaps)

-- void clang_remap_getFilenames(CXRemapping, unsigned index,
--                               CXString *original, CXString *transformed);
{# fun clang_remap_getFilenames { id `Ptr ()', `Int', id `Ptr ()', id `Ptr ()' } -> `()' #}
unsafe_remap_getFilenames :: Remapping s -> Int -> IO (ClangString (), ClangString ())
unsafe_remap_getFilenames remaps idx = do
    origPtr <- mallocBytes (sizeOf (undefined :: (Ptr (ClangString ()))))
    txPtr <- mallocBytes (sizeOf (undefined :: (Ptr (ClangString ()))))
    clang_remap_getFilenames (unRemapping remaps) idx origPtr txPtr
    orig <- peek (castPtr origPtr)
    tx <- peek (castPtr txPtr)
    free origPtr
    free txPtr
    return (orig, tx)

remap_getFilenames :: ClangBase m => Remapping s' -> Int -> ClangT s m (ClangString s, ClangString s)
remap_getFilenames r idx = do
  (orig, tr) <- liftIO $ unsafe_remap_getFilenames r idx
  (,) <$> registerClangString (return orig) <*> registerClangString (return tr)
