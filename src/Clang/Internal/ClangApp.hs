{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clang.Internal.ClangApp
( ClangApp
, runClangApp
, getIndex
, getTranslationUnit
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Foreign.ForeignPtr

import qualified Clang.Internal.FFI as FFI

data ClangEnv = ClangEnv {
    index :: FFI.Index,
    translationUnit  :: FFI.TranslationUnit
  } deriving (Eq, Show)

newtype ClangApp a = ClangApp (StateT ClangEnv IO a)
  deriving (Applicative, Functor, Monad, MonadState ClangEnv, MonadIO)

runClangApp :: ClangApp a -> FFI.IndexPtr -> FFI.TranslationUnitPtr -> IO a
runClangApp (ClangApp st) iPtr tuPtr =  withForeignPtr iPtr $ \i -> 
                                          withForeignPtr tuPtr $ \tu ->
                                            evalStateT st (ClangEnv i tu)

getIndex :: ClangApp FFI.Index
getIndex = get >>= return . index

getTranslationUnit :: ClangApp FFI.TranslationUnit
getTranslationUnit = get >>= return . translationUnit
