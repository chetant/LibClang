#include <clang-c/Index.h>
#include <clang-c/Documentation.h>

// This file contains wrappers around clang_* functions that return
// bare structs. Since bare structs are not supported by the Haskell
// FFI these functions
// 1. call the clang_* function
// 2. copy the returned struct to the heap
// 3. hand the caller a pointer to that location.

#define WRAPPED_HEADER(RETURN_TYPE, CLANG_FUNCTION, CLANG_FUNCTION_ARGS) \
  RETURN_TYPE* wrapped_##CLANG_FUNCTION CLANG_FUNCTION_ARGS
#define WRAPPED_IMPL(RETURN_TYPE, CLANG_FUNCTION, CLANG_FUNCTION_ARGS, CLANG_FUNCTION_CALL_ARGS) \
  RETURN_TYPE* wrapped_##CLANG_FUNCTION CLANG_FUNCTION_ARGS \
  {                                                         \
    RETURN_TYPE* ret = (void*) malloc(sizeof(RETURN_TYPE)); \
    *ret = CLANG_FUNCTION CLANG_FUNCTION_CALL_ARGS;         \
    return ret;                                             \
  };                                                        \

WRAPPED_HEADER(CXSourceLocation, clang_getNullLocation, (void));
WRAPPED_HEADER(CXSourceLocation, clang_getLocation, (CXTranslationUnit tu,
                                                  CXFile file,
                                                  unsigned line,
                                                     unsigned column));
WRAPPED_HEADER(CXSourceLocation, clang_getLocationForOffset, (CXTranslationUnit tu,
                                                           CXFile file,
                                                              unsigned offset));
WRAPPED_HEADER(CXSourceLocation, clang_getRangeStart, (CXSourceRange range));
WRAPPED_HEADER(CXSourceLocation, clang_getRangeEnd, (CXSourceRange range));
WRAPPED_HEADER(CXSourceLocation, clang_getDiagnosticLocation, (CXDiagnostic d));
WRAPPED_HEADER(CXSourceLocation, clang_getCursorLocation, (CXCursor c));
WRAPPED_HEADER(CXSourceLocation, clang_getTokenLocation, (CXTranslationUnit tu,
                                                          CXToken token));

WRAPPED_HEADER(CXSourceRange,clang_getNullRange, (void));
WRAPPED_HEADER(CXSourceRange,clang_getRange,(CXSourceLocation begin,
                                             CXSourceLocation end));
WRAPPED_HEADER(CXSourceRange,clang_getDiagnosticRange,(CXDiagnostic Diagnostic,
                                                      unsigned Range));
WRAPPED_HEADER(CXSourceRange,clang_getCursorExtent,(CXCursor));
WRAPPED_HEADER(CXSourceRange,clang_Cursor_getSpellingNameRange,(CXCursor,
                                                               unsigned pieceIndex,
                                                               unsigned options));
WRAPPED_HEADER(CXSourceRange,clang_Cursor_getCommentRange,(CXCursor C));
WRAPPED_HEADER(CXSourceRange,clang_getCursorReferenceNameRange,(CXCursor C,
                                                               unsigned NameFlags,
                                                               unsigned PieceIndex));
WRAPPED_HEADER(CXSourceRange,clang_getTokenExtent,(CXTranslationUnit, CXToken));

WRAPPED_HEADER(CXCursor,clang_getNullCursor,(void));
WRAPPED_HEADER(CXCursor,clang_getTranslationUnitCursor,(CXTranslationUnit));
WRAPPED_HEADER(CXCursor,clang_getCursorSemanticParent,(CXCursor cursor));
WRAPPED_HEADER(CXCursor,clang_getCursorLexicalParent,(CXCursor cursor));
WRAPPED_HEADER(CXCursor,clang_getCursor,(CXTranslationUnit, CXSourceLocation));
WRAPPED_HEADER(CXCursor,clang_Cursor_getArgument,(CXCursor C, unsigned i));
WRAPPED_HEADER(CXCursor,clang_getTypeDeclaration,(CXType T));
WRAPPED_HEADER(CXCursor,clang_getOverloadedDecl,(CXCursor cursor,
                                                unsigned index));
WRAPPED_HEADER(CXCursor,clang_getCursorReferenced,(CXCursor));
WRAPPED_HEADER(CXCursor,clang_getCursorDefinition,(CXCursor));
WRAPPED_HEADER(CXCursor,clang_getCanonicalCursor,(CXCursor));
WRAPPED_HEADER(CXCursor,clang_getSpecializedCursorTemplate,(CXCursor C));

WRAPPED_HEADER(CXComment,clang_Cursor_getParsedComment,(CXCursor C));

WRAPPED_HEADER(CXType,clang_getCursorType,(CXCursor C));
WRAPPED_HEADER(CXType,clang_getTypedefDeclUnderlyingType,(CXCursor C));
WRAPPED_HEADER(CXType,clang_getEnumDeclIntegerType,(CXCursor C));
WRAPPED_HEADER(CXType,clang_getCanonicalType,(CXType T));
WRAPPED_HEADER(CXType,clang_getPointeeType,(CXType T));
WRAPPED_HEADER(CXType,clang_getResultType,(CXType T));
WRAPPED_HEADER(CXType,clang_getArgType,(CXType T, unsigned i));
WRAPPED_HEADER(CXType,clang_getCursorResultType,(CXCursor C));
WRAPPED_HEADER(CXType,clang_getElementType,(CXType T));
WRAPPED_HEADER(CXType,clang_getArrayElementType,(CXType T));
WRAPPED_HEADER(CXType,clang_Type_getClassType,(CXType T));
WRAPPED_HEADER(CXType,clang_getIBOutletCollectionType,(CXCursor));
WRAPPED_HEADER(CXType,clang_Cursor_getReceiverType,(CXCursor C));

WRAPPED_HEADER(CXString,clang_getFileName,(CXFile SFile));
WRAPPED_HEADER(CXString,clang_formatDiagnostic,(CXDiagnostic Diagnostic,
                                               unsigned Options));
WRAPPED_HEADER(CXString,clang_getDiagnosticSpelling,(CXDiagnostic d));
WRAPPED_HEADER(CXString,clang_getDiagnosticOption,(CXDiagnostic Diag,
                                                  CXString *Disable));
WRAPPED_HEADER(CXString,clang_getDiagnosticCategoryText,(CXDiagnostic d));
WRAPPED_HEADER(CXString,clang_getDiagnosticFixIt,(CXDiagnostic Diagnostic,
                                                 unsigned FixIt,
                                               CXSourceRange *ReplacementRange));
WRAPPED_HEADER(CXString,clang_getTranslationUnitSpelling,(CXTranslationUnit CTUnit));;
WRAPPED_HEADER(CXString,clang_getTypeSpelling,(CXType CT));
WRAPPED_HEADER(CXString,clang_getDeclObjCTypeEncoding,(CXCursor C));
WRAPPED_HEADER(CXString,clang_getTypeKindSpelling,(enum CXTypeKind K));
WRAPPED_HEADER(CXString,clang_getCursorUSR,(CXCursor c));
WRAPPED_HEADER(CXString,clang_constructUSR_ObjCClass,(const char *class_name));
WRAPPED_HEADER(CXString,clang_constructUSR_ObjCCategory,(const char *class_name,
                                 const char *category_name));;
WRAPPED_HEADER(CXString,clang_constructUSR_ObjCProtocol,(const char *protocol_name));;
WRAPPED_HEADER(CXString,clang_constructUSR_ObjCIvar,(const char *name,
                                                    CXString classUSR));
WRAPPED_HEADER(CXString,clang_constructUSR_ObjCMethod,(const char *name,
                                                      unsigned isInstanceMethod,
                                                      CXString classUSR));
WRAPPED_HEADER(CXString,clang_constructUSR_ObjCProperty,(const char *property,
                                                        CXString classUSR));
WRAPPED_HEADER(CXString,clang_getCursorSpelling,(CXCursor c));
WRAPPED_HEADER(CXString,clang_getCursorDisplayName,(CXCursor c));
WRAPPED_HEADER(CXString,clang_Cursor_getRawCommentText,(CXCursor C));
WRAPPED_HEADER(CXString,clang_Cursor_getBriefCommentText,(CXCursor C));
WRAPPED_HEADER(CXString,clang_Module_getName,(CXModule Module));
WRAPPED_HEADER(CXString,clang_Module_getFullName,(CXModule Module));
WRAPPED_HEADER(CXString,clang_TextComment_getText,(CXComment Comment));
WRAPPED_HEADER(CXString,clang_HTMLTagComment_getTagName,(CXComment Comment));
WRAPPED_HEADER(CXString,clang_VerbatimLineComment_getText,(CXComment Comment));
WRAPPED_HEADER(CXString,clang_HTMLTagComment_getAsString,(CXComment Comment));
WRAPPED_HEADER(CXString,clang_FullComment_getAsHTML,(CXComment Comment));
WRAPPED_HEADER(CXString,clang_FullComment_getAsXML,(CXComment Comment));
WRAPPED_HEADER(CXString,clang_getTokenSpelling,(CXTranslationUnit tu, CXToken token));
WRAPPED_HEADER(CXString,clang_getCursorKindSpelling,(enum CXCursorKind Kind));
WRAPPED_HEADER(CXString,clang_getCompletionChunkText,(CXCompletionString completion_string, unsigned chunk_number));;
WRAPPED_HEADER(CXString,clang_getCompletionAnnotation,(CXCompletionString completion_string, unsigned annotation_number));;
WRAPPED_HEADER(CXString,clang_getCompletionParent,(CXCompletionString completion_string, enum CXCursorKind *kind));;
WRAPPED_HEADER(CXString,clang_getCompletionBriefComment,(CXCompletionString completion_string));;
WRAPPED_HEADER(CXString,clang_getClangVersion,());
