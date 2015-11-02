{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Functions for manipulating indexes.
--
-- An index is a container that holds a set of translation units. Generally
-- you'll create an index using 'withNew' and then use that index with functions
-- from "Clang.TranslationUnit" to create and work with translation units.
--
-- This module is intended to be imported qualified.
module Clang.Index
(
-- * Creating an index
  withNew

-- * Index options
, getGlobalOptions
, setGlobalOptions
, FFI.GlobalIndexOptions(..)
, FFI.threadBackgroundPriorityForAll
) where

import Control.Monad.IO.Class

import Clang.Internal.BitFlags
import qualified Clang.Internal.FFI as FFI
import Clang.Internal.Monad

-- | Creates an index.
withNew :: ClangBase m
        => Bool  -- ^ Whether to exclude declarations coming from pre-compiled headers.
        -> Bool  -- ^ Whether to automatically display diagnostic messages.
        -> (forall s. FFI.Index s -> ClangT s m a)  -- ^ The function which will use the index.
        -> m a
withNew i1 i2 f = runClangT (FFI.createIndex i1 i2 >>= f)

-- | Retrieves the global options for this index.
getGlobalOptions :: ClangBase m => FFI.Index s' -> ClangT s m [FFI.GlobalIndexOptions]
getGlobalOptions i = unFlags <$> liftIO (FFI.cXIndex_getGlobalOptions i)

-- | Sets the global options for this index.
setGlobalOptions :: ClangBase m => FFI.Index s' -> [FFI.GlobalIndexOptions] -> ClangT s m ()
setGlobalOptions i opts = liftIO $ FFI.cXIndex_setGlobalOptions i (orFlags opts)
