#include <stdlib.h>

#include "visitors.h"

#define INITIAL_LIST_CAPACITY 16
#define LIST_GROWTH_RATE 4

typedef struct
{
  CXCursor* items;
  unsigned  count;
  size_t    capacity;
} CursorList;

void freeCursorList(CXCursor* cursors)
{
  free(cursors);
}

#define LIST_INIT(_type) {                         \
    malloc(INITIAL_LIST_CAPACITY * sizeof(_type)), \
    0,                                             \
    INITIAL_LIST_CAPACITY                          \
  }

#define LIST_APPEND(_type, _list, _item)                       \
  do {                                                         \
    if (_list->count >= _list->capacity) {                     \
      size_t newCapacity = LIST_GROWTH_RATE * _list->capacity; \
      _list->items =                                           \
        realloc(_list->items, newCapacity * sizeof(_type));    \
      _list->capacity = newCapacity;                           \
    }                                                          \
                                                               \
    _list->items[_list->count++] = _item;                      \
  } while(0)                                                   \

enum CXChildVisitResult childListBuilder(CXCursor cursor, CXCursor parent,
                                         CXClientData clientData)
{
  CursorList* childList = (CursorList*) clientData;
  LIST_APPEND(CXCursor, childList, cursor);
  return CXChildVisit_Continue;
}

void getChildren(CXCursor parent, CXCursor** childrenOut, unsigned* countOut)
{
  CursorList childList = LIST_INIT(CXCursor);
  clang_visitChildren(parent, childListBuilder, &childList);
  *childrenOut = childList.items;
  *countOut = childList.count;
}

enum CXChildVisitResult descendantListBuilder(CXCursor cursor, CXCursor parent,
                                              CXClientData clientData)
{
  CursorList* childList = (CursorList*) clientData;
  LIST_APPEND(CXCursor, childList, cursor);
  return CXChildVisit_Recurse;
}

void getDescendants(CXCursor parent, CXCursor** childrenOut, unsigned* countOut)
{
  CursorList childList = LIST_INIT(CXCursor);
  clang_visitChildren(parent, descendantListBuilder, &childList);
  *childrenOut = childList.items;
  *countOut = childList.count;
}

enum CXChildVisitResult declarationListBuilder(CXCursor cursor, CXCursor parent,
                                               CXClientData clientData)
{
  if (clang_isDeclaration(clang_getCursorKind(cursor))) {
    CursorList* childList = (CursorList*) clientData;
    LIST_APPEND(CXCursor, childList, cursor);
  }
  return CXChildVisit_Recurse;
}

void getDeclarations(CXTranslationUnit tu, CXCursor** childrenOut, unsigned* countOut)
{
  CursorList childList = LIST_INIT(CXCursor);
  CXCursor parent = clang_getTranslationUnitCursor(tu);
  clang_visitChildren(parent, declarationListBuilder, &childList);
  *childrenOut = childList.items;
  *countOut = childList.count;
}

enum CXChildVisitResult referenceListBuilder(CXCursor cursor, CXCursor parent,
                                              CXClientData clientData)
{
  if (clang_isReference(clang_getCursorKind(cursor))) {
    CursorList* childList = (CursorList*) clientData;
    LIST_APPEND(CXCursor, childList, cursor);
  }
  return CXChildVisit_Recurse;
}

void getReferences(CXTranslationUnit tu, CXCursor** childrenOut, unsigned* countOut)
{
  CursorList childList = LIST_INIT(CXCursor);
  CXCursor parent = clang_getTranslationUnitCursor(tu);
  clang_visitChildren(parent, referenceListBuilder, &childList);
  *childrenOut = childList.items;
  *countOut = childList.count;
}

typedef struct
{
  CursorList decls;
  CursorList refs;
} CursorListPair;

enum CXChildVisitResult declAndRefListBuilder(CXCursor cursor, CXCursor parent,
                                              CXClientData clientData)
{
  CursorListPair* declsAndRefs = (CursorListPair*) clientData;

  if (clang_isDeclaration(clang_getCursorKind(cursor))) {
    CursorList* decls = &declsAndRefs->decls;
    LIST_APPEND(CXCursor, decls, cursor);
  }

  if (clang_isReference(clang_getCursorKind(cursor))) {
    CursorList* refs = &declsAndRefs->refs;
    LIST_APPEND(CXCursor, refs, cursor);
  }
  
  return CXChildVisit_Recurse;
}

void getDeclarationsAndReferences(CXTranslationUnit tu,
                                  CXCursor** declsOut,
                                  unsigned* declCountOut,
                                  CXCursor** refsOut,
                                  unsigned* refCountOut)
{
  CursorListPair declsAndRefs = {
    LIST_INIT(CXCursor),
    LIST_INIT(CXCursor)
  };

  CXCursor parent = clang_getTranslationUnitCursor(tu);
  clang_visitChildren(parent, declAndRefListBuilder, &declsAndRefs);

  *declsOut = declsAndRefs.decls.items;
  *declCountOut = declsAndRefs.decls.count;
  *refsOut = declsAndRefs.refs.items;
  *refCountOut = declsAndRefs.refs.count;
}

typedef struct
{
  ParentedCursor* items;
  unsigned        count;
  size_t          capacity;
} ParentedCursorList;

void freeParentedCursorList(ParentedCursor* parentedCursors)
{
  free(parentedCursors);
}

enum CXChildVisitResult parentedDescendantListBuilder(CXCursor cursor, CXCursor parent,
                                                      CXClientData clientData)
{
  ParentedCursorList* descendantList = (ParentedCursorList*) clientData;

  ParentedCursor newEntry = {
    parent,
    cursor
  };

  LIST_APPEND(ParentedCursor, descendantList, newEntry);

  return CXChildVisit_Recurse;
}

void getParentedDescendants(CXCursor parent, ParentedCursor** descendantsOut,
                            unsigned* countOut)
{
  ParentedCursorList descendantList = LIST_INIT(ParentedCursor);
  clang_visitChildren(parent, parentedDescendantListBuilder, &descendantList);
  *descendantsOut = descendantList.items;
  *countOut = descendantList.count;
}

enum CXChildVisitResult parentedDeclarationListBuilder(CXCursor cursor, CXCursor parent,
                                                      CXClientData clientData)
{
  ParentedCursorList* declarationList = (ParentedCursorList*) clientData;

  if (clang_isDeclaration(clang_getCursorKind(cursor))) {
    ParentedCursor newEntry = {
      parent,
      cursor
    };

    LIST_APPEND(ParentedCursor, declarationList, newEntry);
  }

  return CXChildVisit_Recurse;
}

void getParentedDeclarations(CXTranslationUnit tu, ParentedCursor** declarationsOut,
                             unsigned* countOut)
{
  ParentedCursorList declarationList = LIST_INIT(ParentedCursor);
  CXCursor parent = clang_getTranslationUnitCursor(tu);
  clang_visitChildren(parent, parentedDeclarationListBuilder, &declarationList);
  *declarationsOut = declarationList.items;
  *countOut = declarationList.count;
}

enum CXChildVisitResult parentedReferenceListBuilder(CXCursor cursor, CXCursor parent,
                                                      CXClientData clientData)
{
  ParentedCursorList* referenceList = (ParentedCursorList*) clientData;

  if (clang_isReference(clang_getCursorKind(cursor))) {
    ParentedCursor newEntry = {
      parent,
      cursor
    };

    LIST_APPEND(ParentedCursor, referenceList, newEntry);
  }

  return CXChildVisit_Recurse;
}

void getParentedReferences(CXTranslationUnit tu, ParentedCursor** referencesOut,
                           unsigned* countOut)
{
  ParentedCursorList referenceList = LIST_INIT(ParentedCursor);
  CXCursor parent = clang_getTranslationUnitCursor(tu);
  clang_visitChildren(parent, parentedReferenceListBuilder, &referenceList);
  *referencesOut = referenceList.items;
  *countOut = referenceList.count;
}

typedef struct
{
  ParentedCursorList decls;
  ParentedCursorList refs;
} ParentedCursorListPair;

enum CXChildVisitResult parentedDeclAndRefListBuilder(CXCursor cursor,
                                                      CXCursor parent,
                                                      CXClientData clientData)
{
  ParentedCursorListPair* declsAndRefs = (ParentedCursorListPair*) clientData;

  if (clang_isDeclaration(clang_getCursorKind(cursor))) {
    ParentedCursor newEntry = {
      parent,
      cursor
    };

    ParentedCursorList* decls = &declsAndRefs->decls;
    LIST_APPEND(ParentedCursor, decls, newEntry);
  }

  if (clang_isReference(clang_getCursorKind(cursor))) {
    ParentedCursor newEntry = {
      parent,
      cursor
    };

    ParentedCursorList* refs = &declsAndRefs->refs;
    LIST_APPEND(ParentedCursor, refs, newEntry);
  }

  return CXChildVisit_Recurse;
}

void getParentedDeclarationsAndReferences(CXTranslationUnit tu,
                                          ParentedCursor** declsOut,
                                          unsigned* declCountOut,
                                          ParentedCursor** refsOut,
                                          unsigned* refCountOut)
{
  ParentedCursorListPair declsAndRefs = {
    LIST_INIT(ParentedCursor),
    LIST_INIT(ParentedCursor)
  };
 
  CXCursor parent = clang_getTranslationUnitCursor(tu);
  clang_visitChildren(parent, parentedDeclAndRefListBuilder, &declsAndRefs);
  *declsOut = declsAndRefs.decls.items;
  *declCountOut = declsAndRefs.decls.count;
  *refsOut = declsAndRefs.refs.items;
  *refCountOut = declsAndRefs.refs.count;
}

typedef struct
{
  Inclusion* items;
  unsigned   count;
  size_t     capacity;
} InclusionList;

void freeInclusionList(Inclusion* inclusions)
{
  free(inclusions);
}

void inclusionListBuilder(CXFile includedFile, CXSourceLocation* inclusionStack,
                          unsigned stackLen, CXClientData clientData)
{
  // Skip the initial file.
  if (stackLen == 0)
    return;
  
  InclusionList* inclusionList = (InclusionList*) clientData;

  Inclusion newEntry = {
    includedFile,
    inclusionStack[0],
    stackLen == 1  // It's a direct inclusion if there's only one stack entry.
  };

  LIST_APPEND(Inclusion, inclusionList, newEntry);
}

void getInclusions(CXTranslationUnit tu, Inclusion** inclusionsOut,
                   unsigned* countOut)
{
  InclusionList inclusionList = LIST_INIT(Inclusion);
  clang_getInclusions(tu, inclusionListBuilder, &inclusionList);
  *inclusionsOut = inclusionList.items;
  *countOut = inclusionList.count;
}
