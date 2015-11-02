#include "wrappers.h"
#include <stdlib.h>

WRAPPED_IMPL(CXSourceLocation, clang_getNullLocation, (), ());
WRAPPED_IMPL(CXSourceLocation, clang_getLocation, (CXTranslationUnit tu,
                                                   CXFile file,
                                                   unsigned line,
                                                   unsigned column), (tu,
                                                                      file,
                                                                      line,
                                                                      column));
WRAPPED_IMPL(CXSourceLocation, clang_getLocationForOffset, (CXTranslationUnit tu,
                                                            CXFile file,
                                                            unsigned offset), (tu,
                                                                               file,
                                                                               offset));
WRAPPED_IMPL(CXSourceLocation, clang_getRangeStart, (CXSourceRange range), (range));
WRAPPED_IMPL(CXSourceLocation, clang_getRangeEnd, (CXSourceRange range), (range));
WRAPPED_IMPL(CXSourceLocation, clang_getDiagnosticLocation, (CXDiagnostic d), (d));
WRAPPED_IMPL(CXSourceLocation, clang_getCursorLocation, (CXCursor c), (c));
WRAPPED_IMPL(CXSourceLocation, clang_getTokenLocation, (CXTranslationUnit tu,
                                                        CXToken token), (tu,
                                                                         token));

WRAPPED_IMPL(CXSourceRange,clang_getNullRange, (), ());
WRAPPED_IMPL(CXSourceRange,clang_getRange,(CXSourceLocation begin,
                                           CXSourceLocation end),(begin,
                                                                  end));
WRAPPED_IMPL(CXSourceRange,clang_getDiagnosticRange, (CXDiagnostic Diagnostic,
                                                      unsigned Range),(Diagnostic,
                                                                       Range));
WRAPPED_IMPL(CXSourceRange,clang_getCursorExtent,(CXCursor c), (c));
WRAPPED_IMPL(CXSourceRange,clang_Cursor_getSpellingNameRange, (CXCursor c,
                                                               unsigned pieceIndex,
                                                               unsigned options),(c,
                                                                                  pieceIndex,
                                                                                  options));
WRAPPED_IMPL(CXSourceRange,clang_Cursor_getCommentRange, (CXCursor C), (C));
WRAPPED_IMPL(CXSourceRange,clang_getCursorReferenceNameRange, (CXCursor C,
                                                               unsigned NameFlags,
                                                               unsigned PieceIndex),(C,
                                                                                     NameFlags,
                                                                                     PieceIndex));
WRAPPED_IMPL(CXSourceRange,clang_getTokenExtent, (CXTranslationUnit tu, CXToken token), (tu, token));

WRAPPED_IMPL(CXCursor,clang_getNullCursor,(),());
WRAPPED_IMPL(CXCursor,clang_getTranslationUnitCursor,(CXTranslationUnit tu),(tu));
WRAPPED_IMPL(CXCursor,clang_getCursorSemanticParent,(CXCursor cursor),(cursor));
WRAPPED_IMPL(CXCursor,clang_getCursorLexicalParent,(CXCursor cursor),(cursor));
WRAPPED_IMPL(CXCursor,clang_getCursor,(CXTranslationUnit tu, CXSourceLocation sl),(tu, sl));
WRAPPED_IMPL(CXCursor,clang_Cursor_getArgument,(CXCursor C, unsigned i),(C, i));
WRAPPED_IMPL(CXCursor,clang_getTypeDeclaration,(CXType T),(T));
WRAPPED_IMPL(CXCursor,clang_getOverloadedDecl,(CXCursor cursor,
                                                unsigned index),(cursor, index));
WRAPPED_IMPL(CXCursor,clang_getCursorReferenced,(CXCursor c),(c));
WRAPPED_IMPL(CXCursor,clang_getCursorDefinition,(CXCursor c),(c));
WRAPPED_IMPL(CXCursor,clang_getCanonicalCursor,(CXCursor c),(c));
WRAPPED_IMPL(CXCursor,clang_getSpecializedCursorTemplate,(CXCursor C),(C));

WRAPPED_IMPL(CXComment,clang_Cursor_getParsedComment,(CXCursor C),(C));

WRAPPED_IMPL(CXType,clang_getCursorType,(CXCursor C),(C));
WRAPPED_IMPL(CXType,clang_getTypedefDeclUnderlyingType,(CXCursor C),(C));
WRAPPED_IMPL(CXType,clang_getEnumDeclIntegerType,(CXCursor C),(C));
WRAPPED_IMPL(CXType,clang_getCanonicalType,(CXType T),(T));
WRAPPED_IMPL(CXType,clang_getPointeeType,(CXType T),(T));
WRAPPED_IMPL(CXType,clang_getResultType,(CXType T),(T));
WRAPPED_IMPL(CXType,clang_getArgType,(CXType T, unsigned i),(T, i));
WRAPPED_IMPL(CXType,clang_getCursorResultType,(CXCursor C),(C));
WRAPPED_IMPL(CXType,clang_getElementType,(CXType T),(T));
WRAPPED_IMPL(CXType,clang_getArrayElementType,(CXType T),(T));
WRAPPED_IMPL(CXType,clang_Type_getClassType,(CXType T),(T));
WRAPPED_IMPL(CXType,clang_getIBOutletCollectionType,(CXCursor c),(c));
WRAPPED_IMPL(CXType,clang_Cursor_getReceiverType,(CXCursor C),(C));

WRAPPED_IMPL(CXString,clang_getFileName,(CXFile SFile),(SFile));
WRAPPED_IMPL(CXString,clang_formatDiagnostic,(CXDiagnostic Diagnostic,
                                               unsigned Options),(Diagnostic,
                                               Options));
WRAPPED_IMPL(CXString,clang_getDiagnosticSpelling,(CXDiagnostic d),(d));
WRAPPED_IMPL(CXString,clang_getDiagnosticOption,(CXDiagnostic Diag,
                                                  CXString *Disable),(Diag,
                                                  Disable));
WRAPPED_IMPL(CXString,clang_getDiagnosticCategoryText,(CXDiagnostic d),(d));
WRAPPED_IMPL(CXString,clang_getDiagnosticFixIt,(CXDiagnostic Diagnostic,
                                                 unsigned FixIt,
                                               CXSourceRange *ReplacementRange),(Diagnostic,
                                                 FixIt,
                                               ReplacementRange));
WRAPPED_IMPL(CXString,clang_getTranslationUnitSpelling,(CXTranslationUnit CTUnit),(CTUnit));;
WRAPPED_IMPL(CXString,clang_getTypeSpelling,(CXType CT),(CT));
WRAPPED_IMPL(CXString,clang_getDeclObjCTypeEncoding,(CXCursor C),(C));
WRAPPED_IMPL(CXString,clang_getTypeKindSpelling,(enum CXTypeKind K),(K));
WRAPPED_IMPL(CXString,clang_getCursorUSR,(CXCursor c),(c));
WRAPPED_IMPL(CXString,clang_constructUSR_ObjCClass,(const char *class_name),(class_name));
WRAPPED_IMPL(CXString,clang_constructUSR_ObjCCategory,(const char *class_name,
                                 const char *category_name),(class_name,
                                 category_name));;
WRAPPED_IMPL(CXString,clang_constructUSR_ObjCProtocol,(const char *protocol_name),(protocol_name));;
WRAPPED_IMPL(CXString,clang_constructUSR_ObjCIvar,(const char *name,
                                                    CXString classUSR),(name,
                                                    classUSR));
WRAPPED_IMPL(CXString,clang_constructUSR_ObjCMethod,(const char *name,
                                                      unsigned isInstanceMethod,
                                                      CXString classUSR),(name,
                                                      isInstanceMethod,
                                                      classUSR));
WRAPPED_IMPL(CXString,clang_constructUSR_ObjCProperty,(const char *property,
                                                        CXString classUSR),(property,
                                                        classUSR));
WRAPPED_IMPL(CXString,clang_getCursorSpelling,(CXCursor c),(c));
WRAPPED_IMPL(CXString,clang_getCursorDisplayName,(CXCursor c),(c));
WRAPPED_IMPL(CXString,clang_Cursor_getRawCommentText,(CXCursor C),(C));
WRAPPED_IMPL(CXString,clang_Cursor_getBriefCommentText,(CXCursor C),(C));
WRAPPED_IMPL(CXString,clang_Module_getName,(CXModule Module),(Module));
WRAPPED_IMPL(CXString,clang_Module_getFullName,(CXModule Module),(Module));
WRAPPED_IMPL(CXString,clang_TextComment_getText,(CXComment Comment),(Comment));
WRAPPED_IMPL(CXString,clang_HTMLTagComment_getTagName,(CXComment Comment),(Comment));
WRAPPED_IMPL(CXString,clang_VerbatimLineComment_getText,(CXComment Comment),(Comment));
WRAPPED_IMPL(CXString,clang_HTMLTagComment_getAsString,(CXComment Comment),(Comment));
WRAPPED_IMPL(CXString,clang_FullComment_getAsHTML,(CXComment Comment),(Comment));
WRAPPED_IMPL(CXString,clang_FullComment_getAsXML,(CXComment Comment),(Comment));
WRAPPED_IMPL(CXString,clang_getTokenSpelling,(CXTranslationUnit tu, CXToken token),(tu, token));
WRAPPED_IMPL(CXString,clang_getCursorKindSpelling,(enum CXCursorKind Kind),(Kind));
WRAPPED_IMPL(CXString,clang_getCompletionChunkText,(CXCompletionString completion_string, unsigned chunk_number),(completion_string, chunk_number));;
WRAPPED_IMPL(CXString,clang_getCompletionAnnotation,(CXCompletionString completion_string, unsigned annotation_number),(completion_string, annotation_number));;
WRAPPED_IMPL(CXString,clang_getCompletionParent,(CXCompletionString completion_string, enum CXCursorKind *kind),(completion_string, kind));;
WRAPPED_IMPL(CXString,clang_getCompletionBriefComment,(CXCompletionString completion_string),(completion_string));;
WRAPPED_IMPL(CXString,clang_getClangVersion,(),());
