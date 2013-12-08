{-# LANGUAGE RankNTypes #-}

module Clang.Monad
( ClangApp
, clangScope
) where

import Control.Monad.Trans (liftIO)

import Clang.Internal.ClangApp (ClangApp, runClangApp)

clangScope :: (forall s. ClangApp s a) -> ClangApp s' a
clangScope app = liftIO $ runClangApp app
{-# INLINEABLE clangScope #-}
