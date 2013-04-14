{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clang.Internal.ClangApp
( ClangApp
, runClangApp
, mkClangAppRunner
, appAllocate
) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource

-- The functions in this module can be used in an unsafe manner if the lifetime
-- of a ClangApp is not bounded by the lifetime of an outer ClangApp, in which
-- it was created, since we don't transfer ownership of resources. Callers must
-- ensure that the lifetime is controlled appropriately. (The plan is to
-- eventually fix this using ST-style phantom types.)

newtype ClangApp a = ClangApp
  { unClangApp :: (ResourceT IO a)
  } deriving (Applicative, Functor, Monad, MonadIO, MonadResource, MonadThrow, MonadUnsafeIO)

runClangApp :: ClangApp a -> IO a
runClangApp app = runResourceT .  unClangApp $ app

mkClangAppRunner :: ClangApp (ClangApp a -> IO a)
mkClangAppRunner = return $ \app -> runResourceT . unClangApp $ app

appAllocate :: IO a -> (a -> IO ()) -> ClangApp (ReleaseKey, a)
appAllocate = allocate
