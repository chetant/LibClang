#include <clang-c/Index.h>

// This file contains utility functions that make implementing the Haskell FFI
// code easier.

unsigned codeCompleteGetNumResults(CXCodeCompleteResults* results);

enum CXCursorKind codeCompleteGetResult(CXCodeCompleteResults* results,
                                        unsigned index,
                                        CXCompletionString* stringOut);
