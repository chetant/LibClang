#include <clang-c/Index.h>

enum CXChildVisitResult visitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
  CXType type = clang_getCursorType(cursor);
  CXString str = clang_getTypeKindSpelling(type.kind);
  printf("Type:%s\n", clang_getCString(str));
  clang_disposeString(str);
  return CXChildVisit_Continue;
}

int main(int argc, char * argv[])
{
  CXIndex index = clang_createIndex(0, 0);
  CXTranslationUnit txUnit = clang_parseTranslationUnit(index, 0, argv, argc, 0, 0, CXTranslationUnit_None);

  CXCursor cur = clang_getTranslationUnitCursor(txUnit);
  clang_visitChildren(cur, visitor, NULL);

  clang_disposeTranslationUnit(txUnit);
  clang_disposeIndex(index);
  return 0;
}
