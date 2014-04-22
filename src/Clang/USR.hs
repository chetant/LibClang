{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for manipulating 'Unified Symbol Resolution' values, or
-- \'USRs\'. USRs are strings that provide an unambiguous reference to a
-- symbol. Any two cursors that refer to the same semantic entity will
-- have the same USR, even if they occur in different translation
-- units. This is very useful when attempting to cross-reference
-- between different source files in a project.
--
-- Most often, using USRs simply means retrieving the USR that
-- corresponds to a 'Clang.Cursor'. You can do this using
-- 'Clang.Cursor.getUSR', and then convert to a Haskell string using
-- the functions in "Clang.String".
--
-- This module is intended to be imported qualified.
module Clang.USR
( createFromObjCClass
, createFromObjCCategory
, createFromObjCProtocol
, createFromObjCIvar
, createFromObjCInstanceMethod
, createFromObjCClassMethod
, createFromObjCProperty
) where

import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Construct a USR for the specified Objective-C class.
createFromObjCClass :: ClangBase m
                    => String  -- ^ A class name.
                    -> ClangT s m (FFI.ClangString s)
createFromObjCClass = FFI.constructUSR_ObjCClass

-- | Construct a USR for the specified Objective-C category.
createFromObjCCategory :: ClangBase m
                       => String  -- ^ A class name.
                       -> String  -- ^ A category name.
                       -> ClangT s m (FFI.ClangString s)
createFromObjCCategory = FFI.constructUSR_ObjCCategory

-- | Construct a USR for the specified Objective-C protocol.
createFromObjCProtocol :: ClangBase m
                       => String  -- ^ A protocol name.
                       -> ClangT s m (FFI.ClangString s)
createFromObjCProtocol = FFI.constructUSR_ObjCProtocol

-- | Construct a USR for the specified Objective-C instance variable,
-- given the USR for its containing class.
createFromObjCIvar :: ClangBase m
                   => String              -- ^ An instance variable name.
                   -> FFI.ClangString s'  -- ^ A class USR.
                   -> ClangT s m (FFI.ClangString s)
createFromObjCIvar = FFI.constructUSR_ObjCIvar

-- | Construct a USR for the specified Objective-C instance method, given the USR
-- for its containing class.
createFromObjCInstanceMethod :: ClangBase m
                             => String              -- ^ A method name.
                             -> FFI.ClangString s'  -- ^ A class USR.
                             -> ClangT s m (FFI.ClangString s)
createFromObjCInstanceMethod name = FFI.constructUSR_ObjCMethod name True

-- | Construct a USR for the specified Objective-C class method, given the USR
-- for its containing class.
createFromObjCClassMethod :: ClangBase m
                          => String              -- ^ A method name.
                          -> FFI.ClangString s'  -- ^ A class USR.
                          -> ClangT s m (FFI.ClangString s)
createFromObjCClassMethod name = FFI.constructUSR_ObjCMethod name False

-- | Construct a USR for the specified Objective-C property, given the USR
-- for its containing class.
createFromObjCProperty :: ClangBase m
                       => String              -- ^ A property name.
                       -> FFI.ClangString s'  -- ^ A class USR.
                       -> ClangT s m (FFI.ClangString s)
createFromObjCProperty = FFI.constructUSR_ObjCProperty
