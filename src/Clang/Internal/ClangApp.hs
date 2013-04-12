{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clang.Internal.ClangApp
( ClangApp
, runClangApp
, mkClangAppRunner
, getIndex
, getTranslationUnit
, appAllocate
) where

import Control.Applicative
import qualified Control.Monad.Reader.Class as R
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Foreign.ForeignPtr

import qualified Clang.Internal.BaseFFI as FFI

data ClangEnv = ClangEnv {
    index :: FFI.Index,
    translationUnit  :: FFI.TranslationUnit
  } deriving (Eq, Show)

newtype ClangApp a = ClangApp {
    unClangApp :: (ResourceT (ReaderT ClangEnv IO) a)
  } deriving (Applicative, Functor, Monad, R.MonadReader ClangEnv, MonadIO, MonadResource, MonadThrow, MonadUnsafeIO)

runClangApp :: ClangApp a -> FFI.IndexPtr -> FFI.TranslationUnitPtr -> IO a
runClangApp app iPtr tuPtr =
  withForeignPtr iPtr $ \i -> 
    withForeignPtr tuPtr $ \tu ->
      flip runReaderT (ClangEnv i tu) .
      runResourceT .
      unClangApp $ app

askClangEnv :: ClangApp ClangEnv
askClangEnv = ClangApp . lift $ ask

mkClangAppRunner :: ClangApp (ClangApp a -> IO a)
mkClangAppRunner = do
  env <- askClangEnv
  return $ \app -> flip runReaderT env .
                   runResourceT .
                   unClangApp $ app

getIndex :: ClangApp FFI.Index
getIndex = askClangEnv >>= return . index

getTranslationUnit :: ClangApp FFI.TranslationUnit
getTranslationUnit = askClangEnv >>= return . translationUnit

appAllocate :: IO a -> (a -> IO ()) -> ClangApp (ReleaseKey, a)
appAllocate = allocate
