module Clang.Internal.FFIConstants
( sizeOfCXCursor
, alignOfCXCursor
, offsetCXCursorKind
, offsetCXCursorXData
, offsetCXCursorP1
, offsetCXCursorP2
, offsetCXCursorP3
, sizeOfParentedCursor
, alignOfParentedCursor
, offsetParentedCursorParent
, offsetParentedCursorCursor
, sizeOfInclusion
, alignOfInclusion
, offsetInclusionInclusion
, offsetInclusionLocation
, offsetInclusionIsDirect
, sizeOfCXSourceLocation
, alignOfCXSourceLocation
, offsetCXSourceLocationP1
, offsetCXSourceLocationP2
, offsetCXSourceLocationData
, sizeOfCXToken
, alignOfCXToken
, offsetCXTokenI1
, offsetCXTokenI2
, offsetCXTokenI3
, offsetCXTokenI4
, offsetCXTokenData
, sizeOfCXUnsavedFile
, alignOfCXUnsavedFile
, offsetCXUnsavedFileFilename
, offsetCXUnsavedFileContents
, offsetCXUnsavedFileContentsLen
) where

#include <stddef.h>
#include "clang/include/clang-c/Index.h"
#include "cbits/visitors.h"

-- CXCursor constants.
sizeOfCXCursor :: Int
sizeOfCXCursor = (#const sizeof(CXCursor)) 
alignOfCXCursor :: Int
alignOfCXCursor = (#const 4)   -- in C11, could use alignof()
offsetCXCursorKind :: Int
offsetCXCursorKind = (#const offsetof(CXCursor, kind)) 
offsetCXCursorXData :: Int
offsetCXCursorXData = (#const offsetof(CXCursor, xdata)) 
offsetCXCursorP1 :: Int
offsetCXCursorP1 = (#const offsetof(CXCursor, data)) 
offsetCXCursorP2 :: Int
offsetCXCursorP2 = (#const offsetof(CXCursor, data) + sizeof(void*)) 
offsetCXCursorP3 :: Int
offsetCXCursorP3 = (#const offsetof(CXCursor, data) + 2 * sizeof(void*)) 

-- ParentedCursor constants.
sizeOfParentedCursor :: Int
sizeOfParentedCursor = (#const sizeof(struct ParentedCursor)) 
alignOfParentedCursor :: Int
alignOfParentedCursor = (#const 4) 
offsetParentedCursorParent :: Int
offsetParentedCursorParent = (#const offsetof(struct ParentedCursor, parent)) 
offsetParentedCursorCursor :: Int
offsetParentedCursorCursor = (#const offsetof(struct ParentedCursor, cursor)) 

-- Inclusion constants.
sizeOfInclusion :: Int
sizeOfInclusion = (#const sizeof(struct Inclusion)) 
alignOfInclusion :: Int
alignOfInclusion = (#const 4) 
offsetInclusionInclusion :: Int
offsetInclusionInclusion = (#const offsetof(struct Inclusion, inclusion)) 
offsetInclusionLocation :: Int
offsetInclusionLocation = (#const offsetof(struct Inclusion, location)) 
offsetInclusionIsDirect :: Int
offsetInclusionIsDirect = (#const offsetof(struct Inclusion, isDirect)) 

-- SourceLocation constants.
sizeOfCXSourceLocation :: Int
sizeOfCXSourceLocation = (#const sizeof(CXSourceLocation)) 
alignOfCXSourceLocation :: Int
alignOfCXSourceLocation = (#const 4) 
offsetCXSourceLocationP1 :: Int
offsetCXSourceLocationP1 = (#const offsetof(CXSourceLocation, ptr_data)) 
offsetCXSourceLocationP2 :: Int
offsetCXSourceLocationP2 = (#const offsetof(CXSourceLocation, ptr_data) + sizeof(void*)) 
offsetCXSourceLocationData :: Int
offsetCXSourceLocationData = (#const offsetof(CXSourceLocation, int_data)) 

-- CXToken constants.
sizeOfCXToken :: Int
sizeOfCXToken = (#const sizeof(CXToken)) 
alignOfCXToken :: Int
alignOfCXToken = (#const 4) 
offsetCXTokenI1 :: Int
offsetCXTokenI1 = (#const offsetof(CXToken, int_data)) 
offsetCXTokenI2 :: Int
offsetCXTokenI2 = (#const offsetof(CXToken, int_data) + sizeof(unsigned)) 
offsetCXTokenI3 :: Int
offsetCXTokenI3 = (#const offsetof(CXToken, int_data) + 2 * sizeof(unsigned)) 
offsetCXTokenI4 :: Int
offsetCXTokenI4 = (#const offsetof(CXToken, int_data) + 3 * sizeof(unsigned)) 
offsetCXTokenData :: Int
offsetCXTokenData = (#const offsetof(CXToken, ptr_data)) 

-- CXUnsavedFile constants.
sizeOfCXUnsavedFile :: Int
sizeOfCXUnsavedFile = (#const sizeof(struct CXUnsavedFile)) 
alignOfCXUnsavedFile :: Int
alignOfCXUnsavedFile = (#const 4) 
offsetCXUnsavedFileFilename :: Int
offsetCXUnsavedFileFilename = (#const offsetof(struct CXUnsavedFile, Filename)) 
offsetCXUnsavedFileContents :: Int
offsetCXUnsavedFileContents = (#const offsetof(struct CXUnsavedFile, Contents)) 
offsetCXUnsavedFileContentsLen :: Int
offsetCXUnsavedFileContentsLen = (#const offsetof(struct CXUnsavedFile, Length)) 
