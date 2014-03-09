#include <stdio.h>
#include <clang-c/Index.h>

enum CXChildVisitResult visitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
  enum CXCursorKind cKind = clang_getCursorKind(cursor);
  CXString nameString = clang_getCursorDisplayName(cursor);
  CXString typeString = clang_getCursorKindSpelling(cKind);
  printf("Name:%s, Kind:%s\n", clang_getCString(nameString), clang_getCString(typeString));
  clang_disposeString(nameString);
  clang_disposeString(typeString);
  return CXChildVisit_Continue;
}

int main(int argc, char * argv[])
{
  CXIndex index = clang_createIndex(0, 0);
  CXTranslationUnit txUnit = clang_parseTranslationUnit(index, 0, (const char * const *) argv, argc, 0, 0, CXTranslationUnit_None);

  CXCursor cur = clang_getTranslationUnitCursor(txUnit);
  clang_visitChildren(cur, visitor, NULL);

  clang_disposeTranslationUnit(txUnit);
  clang_disposeIndex(index);
  return 0;
}
