{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Clang.Internal.Monad
( ClangT
, ClangBase
, ClangValue(..)
, ClangValueList(..)
, Proxy
, runClangT
, clangScope
, clangAllocate
, mkProxy
) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.Vector.Storable as DVS
import Unsafe.Coerce (unsafeCoerce)  -- With GHC 7.8 we can use the safer 'coerce'.

newtype ClangT s m a = ClangT
  { unClangT :: ResourceT m a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadTrans)

instance MonadBase b m => MonadBase b (ClangT s m) where
  liftBase = lift . liftBase

instance ClangBase m => MonadResource (ClangT s m) where
  liftResourceT = transClangT liftIO

transClangT :: (m a -> n b) -> ResourceT m a -> ClangT s n b
transClangT f rt = ClangT $ transResourceT f rt

type ClangBase m = MonadResourceBase m

data Proxy s

runClangT :: ClangBase m => (forall s. ClangT s m a) -> m a
runClangT f = runResourceT . unClangT $ f
{-# INLINEABLE runClangT #-}

-- | Runs a monadic computation with libclang and frees all the
-- resources allocated by that computation immediately.
clangScope :: ClangBase m => (forall s. ClangT s m a) -> ClangT s' m a
clangScope = lift . runClangT
{-# INLINEABLE clangScope #-}

clangAllocate :: ClangBase m => IO a -> (a -> IO ()) -> ClangT s m (ReleaseKey, a)
clangAllocate = allocate
{-# INLINEABLE clangAllocate #-}

mkProxy :: Proxy s
mkProxy = undefined
{-# INLINE mkProxy #-}

class ClangValue v where
  -- | Promotes a value from an outer scope to the current inner scope.
  -- The value's lifetime remains that of the outer scope.
  -- This is never necessary, but it may allow you to write code more naturally in
  -- some situations, since it can occasionally be inconvenient that variables
  -- from different scopes are different types.
  fromOuterScope :: ClangBase m => v s' -> ClangT s m (v s)
  fromOuterScope = return . unsafeCoerce

class ClangValueList v where
  -- | Promotes a list from an outer scope to the current inner scope.
  -- The list's lifetime remains that of the outer scope.
  -- This is never necessary, but it may allow you to write code more naturally in
  -- some situations, since it can occasionally be inconvenient that variables
  -- from different scopes are different types.
  listFromOuterScope :: ClangBase m => DVS.Vector (v s') -> ClangT s m (DVS.Vector (v s))
  listFromOuterScope = return . unsafeCoerce
