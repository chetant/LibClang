#include "utils.h"

unsigned codeCompleteGetNumResults(CXCodeCompleteResults* results)
{
  return results ? results->NumResults
                 : 0;
}

// We don't perform any null or bounds checks here because this is an internal function
// with only one caller, but if we exposed this API publicly we'd have to do so.
enum CXCursorKind codeCompleteGetResult(CXCodeCompleteResults* results,
                                        unsigned index,
                                        CXCompletionString* stringOut)
{
  *stringOut = results->Results[index].CompletionString;
  return results->Results[index].CursorKind;
}
