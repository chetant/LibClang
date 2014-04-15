#ifndef LIBCLANG_VISITORS_H
#define LIBCLANG_VISITORS_H

#include "clang-c/Index.h"

void freeCursorList(CXCursor* cursors);

typedef struct
{
  CXCursor parent;
  CXCursor cursor;
} ParentedCursor;

void freeParentedCursorList(ParentedCursor* parentedCursors);

// Wrappers for libclang traversals.
void getChildren(CXCursor parent, CXCursor** childrenOut, unsigned* countOut);
void getDescendants(CXCursor parent, CXCursor** childrenOut, unsigned* countOut);
void getDeclarations(CXTranslationUnit tu, CXCursor** declsOut, unsigned* declCountOut);
void getReferences(CXTranslationUnit tu, CXCursor** refsOut, unsigned* refCountOut);
void getDeclarationsAndReferences(CXTranslationUnit tu,
                                  CXCursor** declsOut, unsigned* declCountOut,
                                  CXCursor** refsOut, unsigned* refCountOut);

// Variants that include the parent of each cursor.
void getParentedDescendants(CXCursor parent,
                            ParentedCursor** descendantsOut,
                            unsigned* countOut);
void getParentedDeclarations(CXTranslationUnit tu,
                             ParentedCursor** declsOut,
                             unsigned* declCountOut);
void getParentedReferences(CXTranslationUnit tu,
                           ParentedCursor** refsOut,
                           unsigned* refCountOut);
void getParentedDeclarationsAndReferences(CXTranslationUnit tu,
                                          ParentedCursor** declsOut,
                                          unsigned* declCountOut,
                                          ParentedCursor** refsOut,
                                          unsigned* refCountOut);

// Wrappers for clang_getInclusions.
typedef struct
{
  CXFile           inclusion;
  CXSourceLocation location;
  unsigned char    isDirect;
} Inclusion;

void freeInclusionList(Inclusion* inclusions);

void getInclusions(CXTranslationUnit tu, Inclusion** inclusionsOut,
                   unsigned* countOut);

#endif
