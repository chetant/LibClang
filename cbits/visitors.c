#include <stdlib.h>

#include "visitors.h"

#define INITIAL_LIST_CAPACITY 16
#define LIST_GROWTH_RATE 4

struct ChildList
{
  CXCursor* children;
  unsigned  count;
  size_t    capacity;
};

enum CXChildVisitResult childListBuilder(CXCursor cursor, CXCursor parent,
                                         CXClientData clientData)
{
  struct ChildList* childList = (struct ChildList*) clientData;

  // Expand our capacity if necessary.
  if (childList->count >= childList->capacity) {
    size_t newCapacity = LIST_GROWTH_RATE * childList->capacity;
    childList->children = realloc(childList->children, newCapacity * sizeof(CXCursor));
    childList->capacity = newCapacity;
  }

  childList->children[childList->count++] = cursor;
  
  return CXChildVisit_Continue;
}

void getChildren(CXCursor parent, CXCursor** childrenOut, unsigned* countOut)
{
  struct ChildList childList = {
    malloc(INITIAL_LIST_CAPACITY * sizeof(CXCursor)),
    0,
    INITIAL_LIST_CAPACITY
  };

  clang_visitChildren(parent, childListBuilder, &childList);

  *childrenOut = childList.children;
  *countOut = childList.count;
}

enum CXChildVisitResult descendantListBuilder(CXCursor cursor, CXCursor parent,
                                              CXClientData clientData)
{
  struct ChildList* childList = (struct ChildList*) clientData;

  // Expand our capacity if necessary.
  if (childList->count >= childList->capacity) {
    size_t newCapacity = LIST_GROWTH_RATE * childList->capacity;
    childList->children = realloc(childList->children, newCapacity * sizeof(CXCursor));
    childList->capacity = newCapacity;
  }

  childList->children[childList->count++] = cursor;
  
  return CXChildVisit_Recurse;
}

void getDescendants(CXCursor parent, CXCursor** childrenOut, unsigned* countOut)
{
  struct ChildList childList = {
    malloc(INITIAL_LIST_CAPACITY * sizeof(CXCursor)),
    0,
    INITIAL_LIST_CAPACITY
  };

  clang_visitChildren(parent, descendantListBuilder, &childList);

  *childrenOut = childList.children;
  *countOut = childList.count;
}

struct ParentedCursorList
{
  struct ParentedCursor* descendants;
  unsigned               count;
  size_t                 capacity;
};

enum CXChildVisitResult parentedDescendantListBuilder(CXCursor cursor, CXCursor parent,
                                                      CXClientData clientData)
{
  struct ParentedCursorList* descendantList = (struct ParentedCursorList*) clientData;

  // Expand our capacity if necessary.
  if (descendantList->count >= descendantList->capacity) {
    size_t newCapacity = LIST_GROWTH_RATE * descendantList->capacity;
    descendantList->descendants =
      realloc(descendantList->descendants, newCapacity * sizeof(struct ParentedCursor));
    descendantList->capacity = newCapacity;
  }

  struct ParentedCursor newEntry = {
    parent,
    cursor
  };

  descendantList->descendants[descendantList->count++] = newEntry;
  
  return CXChildVisit_Recurse;
}

void getParentedDescendants(CXCursor parent, struct ParentedCursor** descendantsOut,
                            unsigned* countOut)
{
  struct ParentedCursorList descendantList = {
    malloc(INITIAL_LIST_CAPACITY * sizeof(struct ParentedCursor)),
    0,
    INITIAL_LIST_CAPACITY
  };

  clang_visitChildren(parent, parentedDescendantListBuilder, &descendantList);

  *descendantsOut = descendantList.descendants;
  *countOut = descendantList.count;
}

void freeCursorList(CXCursor* cursors)
{
  free(cursors);
}

void freeParentedCursorList(struct ParentedCursor* parentedCursors)
{
  free(parentedCursors);
}

struct InclusionList
{
  struct Inclusion* inclusions;
  unsigned          count;
  size_t            capacity;
};

void inclusionListBuilder(CXFile includedFile, CXSourceLocation* inclusionStack,
                          unsigned stackLen, CXClientData clientData)
{
  // Skip the initial file.
  if (stackLen == 0)
    return;
  
  struct InclusionList* inclusionList = (struct InclusionList*) clientData;

  // Expand our capacity if necessary.
  if (inclusionList->count >= inclusionList->capacity) {
    size_t newCapacity = LIST_GROWTH_RATE * inclusionList->capacity;
    inclusionList->inclusions = realloc(inclusionList->inclusions,
                                        newCapacity * sizeof(struct Inclusion));
    inclusionList->capacity = newCapacity;
  }

  struct Inclusion newEntry = {
    includedFile,
    inclusionStack[0],
    stackLen == 1  // It's a direct inclusion if there's only one stack entry.
  };

  inclusionList->inclusions[inclusionList->count++] = newEntry;
}

void getInclusions(CXTranslationUnit tu, struct Inclusion** inclusionsOut,
                   unsigned* countOut)
{
  struct InclusionList inclusionList = {
    malloc(INITIAL_LIST_CAPACITY * sizeof(struct Inclusion)),
    0,
    INITIAL_LIST_CAPACITY
  };

  clang_getInclusions(tu, inclusionListBuilder, &inclusionList);

  *inclusionsOut = inclusionList.inclusions;
  *countOut = inclusionList.count;
}

void freeInclusionList(struct Inclusion* inclusions)
{
  free(inclusions);
}
