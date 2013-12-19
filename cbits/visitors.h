#ifndef LIBCLANG_VISITORS_H
#define LIBCLANG_VISITORS_H

#include "clang-c/Index.h"

// Wrappers for clang_visitChildren.
void getChildren(CXCursor parent, CXCursor** childrenOut, unsigned* countOut);

void getDescendants(CXCursor parent, CXCursor** childrenOut, unsigned* countOut);

void freeChildren(CXCursor* children);

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
