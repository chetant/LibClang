{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Clang.Internal.Monad
( ClangT
, ClangBase
, Proxy
, runClangT
, clangAllocate
) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Resource

newtype ClangT s m a = ClangT
  { unClangT :: ResourceT m a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadResource, MonadThrow, MonadTrans)

instance MonadBase b m => MonadBase b (ClangT s m) where
  liftBase = lift . liftBase

type ClangBase m = MonadResourceBase m

data Proxy s

runClangT :: ClangBase m => (forall s. ClangT s m a) -> m a
runClangT f = runResourceT . unClangT $ f
{-# INLINEABLE runClangT #-}

clangAllocate :: ClangBase m => IO a -> (a -> IO ()) -> ClangT s m (ReleaseKey, a)
clangAllocate = allocate
{-# INLINEABLE clangAllocate #-}
