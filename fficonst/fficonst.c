#include <stdio.h>
#include <stddef.h>

#include <clang-c/Index.h>
#include "visitors.h"

void genConstant(const char* name, size_t value)
{
  printf("\n");
  printf("%s :: Int\n", name);
  printf("%s = %zu\n", name, value);
}

int main(int argc, char** argv)
{
  // CXCursor constants.
  genConstant("sizeOfCXCursor", sizeof(CXCursor));
  genConstant("alignOfCXCursor", 4);  // in C11, could use alignof()
  genConstant("offsetCXCursorKind", offsetof(CXCursor, kind));
  genConstant("offsetCXCursorXData", offsetof(CXCursor, xdata));
  genConstant("offsetCXCursorP1", offsetof(CXCursor, data));
  genConstant("offsetCXCursorP2", offsetof(CXCursor, data) + sizeof(void*));
  genConstant("offsetCXCursorP3", offsetof(CXCursor, data) + 2 * sizeof(void*));

  // ParentedCursor constants.
  genConstant("sizeOfParentedCursor", sizeof(struct ParentedCursor));
  genConstant("alignOfParentedCursor", 4);
  genConstant("offsetParentedCursorParent", offsetof(struct ParentedCursor, parent));
  genConstant("offsetParentedCursorCursor", offsetof(struct ParentedCursor, cursor));

  // Inclusion constants.
  genConstant("sizeOfInclusion", sizeof(struct Inclusion));
  genConstant("alignOfInclusion", 4);
  genConstant("offsetInclusionInclusion", offsetof(struct Inclusion, inclusion));
  genConstant("offsetInclusionLocation", offsetof(struct Inclusion, location));
  genConstant("offsetInclusionIsDirect", offsetof(struct Inclusion, isDirect));

  // SourceLocation constants.
  genConstant("sizeOfCXSourceLocation", sizeof(CXSourceLocation));
  genConstant("alignOfCXSourceLocation", 4);
  genConstant("offsetCXSourceLocationP1", offsetof(CXSourceLocation, ptr_data));
  genConstant("offsetCXSourceLocationP2", offsetof(CXSourceLocation, ptr_data) + sizeof(void*));
  genConstant("offsetCXSourceLocationData", offsetof(CXSourceLocation, int_data));

  // CXToken constants.
  genConstant("sizeOfCXToken", sizeof(CXToken));
  genConstant("alignOfCXToken", 4);
  genConstant("offsetCXTokenI1", offsetof(CXToken, int_data));
  genConstant("offsetCXTokenI2", offsetof(CXToken, int_data) + sizeof(unsigned));
  genConstant("offsetCXTokenI3", offsetof(CXToken, int_data) + 2 * sizeof(unsigned));
  genConstant("offsetCXTokenI4", offsetof(CXToken, int_data) + 3 * sizeof(unsigned));
  genConstant("offsetCXTokenData", offsetof(CXToken, ptr_data));

  // CXUnsavedFile constants.
  genConstant("sizeOfCXUnsavedFile", sizeof(struct CXUnsavedFile));
  genConstant("alignOfCXUnsavedFile", 4);
  genConstant("offsetCXUnsavedFileFilename", offsetof(struct CXUnsavedFile, Filename));
  genConstant("offsetCXUnsavedFileContents", offsetof(struct CXUnsavedFile, Contents));
  genConstant("offsetCXUnsavedFileContentsLen", offsetof(struct CXUnsavedFile, Length));

  return 0;
}
