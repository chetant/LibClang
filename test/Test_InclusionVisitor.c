#include <clang-c/Index.h>

void inclusionVisitor(CXFile file, CXSourceLocation * srcLocs, unsigned numSrcLocs, CXClientData data)
{
  CXString fname = clang_getFileName(file);
  printf("Included:%s\n",clang_getCString(fname));
  clang_disposeString(fname);
}

int main(int argc, char * argv[])
{
  CXIndex index = clang_createIndex(0, 0);
  CXTranslationUnit txUnit = clang_parseTranslationUnit(index, 0, argv, argc, 0, 0, CXTranslationUnit_None);

  clang_getInclusions(txUnit, inclusionVisitor, NULL);

  clang_disposeTranslationUnit(txUnit);
  clang_disposeIndex(index);
  return 0;
}
