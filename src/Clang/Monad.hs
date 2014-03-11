{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Clang.Monad
( ClangT
, Clang
, ClangBase
, Proxy
, clangScope
, mkProxy
) where

import Control.Monad.Trans (lift)

import Clang.Internal.Monad (ClangBase, ClangT, Proxy, runClangT)

type Clang s a = ClangT s IO a

-- | Runs a monadic computation with libclang and frees all the
-- resources allocated by that computation immediately.
clangScope :: ClangBase m => (forall s. ClangT s m a) -> ClangT s' m a
clangScope = lift . runClangT
{-# INLINEABLE clangScope #-}

mkProxy :: Proxy s
mkProxy = undefined
{-# INLINE mkProxy #-}
