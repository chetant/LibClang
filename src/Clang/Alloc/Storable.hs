{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Clang.Alloc.Storable where

import Foreign.Storable
import Foreign.Marshal.Alloc(malloc, free)
import Foreign.Marshal.Utils(new)

import Clang.Alloc

instance (Storable a) => Alloc a where
    alloc = malloc
    allocSet = new
    dealloc = free
