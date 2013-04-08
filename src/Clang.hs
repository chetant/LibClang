-- | This module reexports all of the Clang.* submodules.
module Clang
( module Clang.Alloc
-- , module Clang.Completion
, module Clang.CrossReference
, module Clang.Cursor
, module Clang.Debug
-- , module Clang.Diagnostic
, module Clang.File
, module Clang.Monad
-- , module Clang.Source
, module Clang.String
-- , module Clang.Token
-- , module Clang.TranslationUnit
, module Clang.Traversal
-- , module Clang.Type
) where

-- FIXME: A _lot_ of conflicting exports right now.

import Clang.Alloc
-- import Clang.Completion
import Clang.CrossReference
import Clang.Cursor
import Clang.Debug
-- import Clang.Diagnostic
import Clang.File
import Clang.Monad
-- import Clang.Source
import Clang.String
-- import Clang.Token
-- import Clang.TranslationUnit
import Clang.Traversal
-- import Clang.Type
