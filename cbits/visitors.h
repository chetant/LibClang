#ifndef LIBCLANG_VISITORS_H
#define LIBCLANG_VISITORS_H

#include "clang-c/Index.h"

// Wrappers for clang_visitChildren.
struct ParentedCursor
{
  CXCursor parent;
  CXCursor cursor;
};

void getChildren(CXCursor parent, CXCursor** childrenOut, unsigned* countOut);

void getDescendants(CXCursor parent, CXCursor** childrenOut, unsigned* countOut);

void getParentedDescendants(CXCursor parent, struct ParentedCursor** descendantsOut,
                            unsigned* countOut);

void freeChildren(CXCursor* children);
void freeParentedDescendants(struct ParentedCursor* descendants);

// Wrappers for clang_getInclusions.
struct Inclusion
{
  CXFile           inclusion;
  CXSourceLocation location;
  unsigned char    isDirect;
};

void getInclusions(CXTranslationUnit tu, struct Inclusion** inclusionsOut,
                   unsigned* countOut);

void freeInclusions(struct Inclusion* inclusions);

#endif
