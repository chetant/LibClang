{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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
, sizeOfCXSourceRange
, alignOfCXSourceRange
, offsetCXSourceRangeP1
, offsetCXSourceRangeP2
, offsetCXSourceRangeBegin
, offsetCXSourceRangeEnd
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
, sizeOfCXComment
, alignOfCXComment
, offsetCXCommentASTNode
, offsetCXCommentTranslationUnit
, sizeOfCXVersion
, alignOfCXVersion
, offsetCXVersionMajor
, offsetCXVersionMinor
, offsetCXVersionSubminor
, sizeOfCXString
, alignOfCXString
, offsetCXStringData
, offsetCXStringFlags
, sizeOfCXPlatformAvailability
, alignOfCXPlatformAvailability
, offsetCXPlatformAvailabilityPlatform
, offsetCXPlatformAvailabilityIntroduced
, offsetCXPlatformAvailabilityDeprecated
, offsetCXPlatformAvailabilityObsoleted
, offsetCXPlatformAvailabilityUnavailable
, offsetCXPlatformAvailabilityMessage
, sizeOfCXCodeCompleteResults
, alignOfCXCodeCompleteResults
, offsetCXCodeCompleteResultsResults
, offsetCXCodeCompleteResultsNumResults
) where

#include <stddef.h>
#include "clang/include/clang-c/Index.h"
#include "cbits/visitors.h"

-- CXCursor constants.
sizeOfCXCursor = (#const sizeof(CXCursor)) :: Int
alignOfCXCursor = (#const 4) :: Int  -- in C11, could use alignof()
offsetCXCursorKind = (#const offsetof(CXCursor, kind)) :: Int
offsetCXCursorXData = (#const offsetof(CXCursor, xdata)) :: Int
offsetCXCursorP1 = (#const offsetof(CXCursor, data)) :: Int
offsetCXCursorP2 = (#const offsetof(CXCursor, data) + sizeof(void*)) :: Int
offsetCXCursorP3 = (#const offsetof(CXCursor, data) + 2 * sizeof(void*)) :: Int

-- ParentedCursor constants.
sizeOfParentedCursor = (#const sizeof(ParentedCursor)) :: Int
alignOfParentedCursor = (#const 4) :: Int
offsetParentedCursorParent = (#const offsetof(ParentedCursor, parent)) :: Int
offsetParentedCursorCursor = (#const offsetof(ParentedCursor, cursor)) :: Int

-- Inclusion constants.
sizeOfInclusion = (#const sizeof(Inclusion)) :: Int
alignOfInclusion = (#const 4) :: Int
offsetInclusionInclusion = (#const offsetof(Inclusion, inclusion)) :: Int
offsetInclusionLocation = (#const offsetof(Inclusion, location)) :: Int
offsetInclusionIsDirect = (#const offsetof(Inclusion, isDirect)) :: Int

-- SourceLocation constants.
sizeOfCXSourceLocation = (#const sizeof(CXSourceLocation)) :: Int
alignOfCXSourceLocation = (#const 4) :: Int
offsetCXSourceLocationP1 = (#const offsetof(CXSourceLocation, ptr_data)) :: Int
offsetCXSourceLocationP2 = (#const offsetof(CXSourceLocation, ptr_data) + sizeof(void*)) :: Int
offsetCXSourceLocationData = (#const offsetof(CXSourceLocation, int_data)) :: Int

-- SourceRange constants.
sizeOfCXSourceRange = (#const sizeof(CXSourceRange)) :: Int
alignOfCXSourceRange = (#const 4) :: Int
offsetCXSourceRangeP1 = (#const offsetof(CXSourceRange, ptr_data)) :: Int
offsetCXSourceRangeP2 = (#const offsetof(CXSourceRange, ptr_data) + sizeof(void*)) :: Int
offsetCXSourceRangeBegin= (#const offsetof(CXSourceRange, begin_int_data)) :: Int
offsetCXSourceRangeEnd = (#const offsetof(CXSourceRange, end_int_data) + sizeof(unsigned)) :: Int

-- CXToken constants.
sizeOfCXToken = (#const sizeof(CXToken)) :: Int
alignOfCXToken = (#const 4) :: Int
offsetCXTokenI1 = (#const offsetof(CXToken, int_data)) :: Int
offsetCXTokenI2 = (#const offsetof(CXToken, int_data) + sizeof(unsigned)) :: Int
offsetCXTokenI3 = (#const offsetof(CXToken, int_data) + 2 * sizeof(unsigned)) :: Int
offsetCXTokenI4 = (#const offsetof(CXToken, int_data) + 3 * sizeof(unsigned)) :: Int
offsetCXTokenData = (#const offsetof(CXToken, ptr_data)) :: Int

-- CXUnsavedFile constants.
sizeOfCXUnsavedFile = (#const sizeof(struct CXUnsavedFile)) :: Int
alignOfCXUnsavedFile = (#const 4) :: Int
offsetCXUnsavedFileFilename = (#const offsetof(struct CXUnsavedFile, Filename)) :: Int
offsetCXUnsavedFileContents = (#const offsetof(struct CXUnsavedFile, Contents)) :: Int
offsetCXUnsavedFileContentsLen = (#const offsetof(struct CXUnsavedFile, Length)) :: Int

-- CXComment constants.
sizeOfCXComment = (#const sizeof(CXComment)) :: Int
alignOfCXComment = (#const 4) :: Int
offsetCXCommentASTNode = (#const offsetof(CXComment, ASTNode)) :: Int
offsetCXCommentTranslationUnit = (#const offsetof(CXComment, TranslationUnit)) :: Int

-- CXVersion constants.
sizeOfCXVersion = (#const sizeof(CXVersion)) :: Int
alignOfCXVersion = (#const 4) :: Int
offsetCXVersionMajor = (#const offsetof(CXVersion, Major)) :: Int
offsetCXVersionMinor = (#const offsetof(CXVersion, Minor)) :: Int
offsetCXVersionSubminor = (#const offsetof(CXVersion, Subminor)) :: Int

-- CXString constants.
sizeOfCXString = (#const sizeof(CXString)) :: Int
alignOfCXString = (#const 4) :: Int
offsetCXStringData = (#const offsetof(CXString, data)) :: Int
offsetCXStringFlags = (#const offsetof(CXString, private_flags)) :: Int

-- CXPlatformAvailability constants.
sizeOfCXPlatformAvailability = (#const sizeof(CXPlatformAvailability)) :: Int
alignOfCXPlatformAvailability = (#const 4) :: Int
offsetCXPlatformAvailabilityPlatform = (#const offsetof(CXPlatformAvailability, Platform)) :: Int
offsetCXPlatformAvailabilityIntroduced = (#const offsetof(CXPlatformAvailability, Introduced)) :: Int
offsetCXPlatformAvailabilityDeprecated = (#const offsetof(CXPlatformAvailability, Deprecated)) :: Int
offsetCXPlatformAvailabilityObsoleted = (#const offsetof(CXPlatformAvailability, Obsoleted)) :: Int
offsetCXPlatformAvailabilityUnavailable = (#const offsetof(CXPlatformAvailability, Unavailable)) :: Int
offsetCXPlatformAvailabilityMessage = (#const offsetof(CXPlatformAvailability, Message)) :: Int

-- CXCodeCompleteResults constants.
sizeOfCXCodeCompleteResults = (#const sizeof(CXString)) :: Int
alignOfCXCodeCompleteResults = (#const 4) :: Int
offsetCXCodeCompleteResultsResults = (#const offsetof(CXCodeCompleteResults, Results)) :: Int
offsetCXCodeCompleteResultsNumResults = (#const offsetof(CXCodeCompleteResults, NumResults)) :: Int
