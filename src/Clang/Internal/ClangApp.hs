{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Clang.Internal.ClangApp
( ClangApp
, runClangApp
, appAllocate
) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource

newtype ClangApp s a = ClangApp
  { unClangApp :: ResourceT IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadResource, MonadThrow, MonadUnsafeIO)

runClangApp :: (forall s. ClangApp s a) -> IO a
runClangApp app = runResourceT .  unClangApp $ app
{-# INLINEABLE runClangApp #-}

appAllocate :: IO a -> (a -> IO ()) -> ClangApp s (ReleaseKey, a)
appAllocate = allocate
{-# INLINEABLE appAllocate #-}
