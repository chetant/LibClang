{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Clang.Monad
( ClangT
, Clang
, ClangBase
, ClangValue(..)
, ClangValueList(..)
, Proxy
, clangScope
) where

import Control.Monad.Trans (lift)

import Clang.Internal.Monad

type Clang s a = ClangT s IO a

-- | Runs a monadic computation with libclang and frees all the
-- resources allocated by that computation immediately.
clangScope :: ClangBase m => (forall s. ClangT s m a) -> ClangT s' m a
clangScope = lift . runClangT
{-# INLINEABLE clangScope #-}
