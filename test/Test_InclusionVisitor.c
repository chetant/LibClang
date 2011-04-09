#include <clang-c/Index.h>

void inclusionVisitor(CXFile file, CXSourceLocation * srcLocs, unsigned numSrcLocs, CXClientData data)
{
  printf("BOOYAH!\n");
}

int main(int argc, char * argv[])
{
  CXIndex index = clang_createIndex(0, 0);
  CXTranslationUnit txUnit = clang_parseTranslationUnit(index, 0, argv, argc, 0, 0, CXTranslationUnit_None);

  clang_getInclusions(txUnit, inclusionVisitor, NULL);
  /* unsigned n = clang_getNumDiagnostics(txUnit); */
  /* printf("numDiags:%d\n", n); */
  /* for(unsigned i = 0, n = clang_getNumDiagnostics(txUnit); i != n; ++i) */
  /* { */
  /*   CXDiagnostic diag = clang_getDiagnostic(txUnit, i); */
  /*   CXString str = clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions()); */
  /*   printf("Diag:%s\n", clang_getCString(str)); */
  /*   clang_disposeString(str); */
  /* } */

  clang_disposeTranslationUnit(txUnit);
  clang_disposeIndex(index);
  return 0;
}
