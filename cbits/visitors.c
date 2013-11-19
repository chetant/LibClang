#include <stdlib.h>

#include "clang-c/Index.h"

#define INITIAL_LIST_CAPACITY 1024

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
    size_t newCapacity = 2 * childList->capacity;
    childList->children = realloc(childList->children, newCapacity);
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

void freeChildren(CXCursor* children)
{
  free(children);
}

struct InclusionEntry
{
  CXFile file;
  char   isDirect;
};

struct InclusionList
{
  struct InclusionEntry* inclusions;
  unsigned               count;
  size_t                 capacity;
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
    size_t newCapacity = 2 * inclusionList->capacity;
    inclusionList->inclusions = realloc(inclusionList->inclusions, newCapacity);
    inclusionList->capacity = newCapacity;
  }

  struct InclusionEntry newEntry = {
    includedFile,
    stackLen == 1  // It's a direct inclusion if there's only one stack entry.
  };

  inclusionList->inclusions[inclusionList->count++] = newEntry;
}

void getInclusions(CXTranslationUnit tu, struct InclusionEntry** inclusionsOut,
                   unsigned* countOut)
{
  struct InclusionList inclusionList = {
    malloc(INITIAL_LIST_CAPACITY * sizeof(struct InclusionEntry)),
    0,
    INITIAL_LIST_CAPACITY
  };

  clang_getInclusions(tu, inclusionListBuilder, &inclusionList);

  *inclusionsOut = inclusionList.inclusions;
  *countOut = inclusionList.count;
}

void freeInclusions(struct InclusionEntry* inclusions)
{
  free(inclusions);
}
