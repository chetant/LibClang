module Clang.Alloc where

import Foreign.Ptr(Ptr(..))

class Alloc a where
    alloc :: IO (Ptr a)
    allocSet :: a -> IO (Ptr a)
    dealloc :: Ptr a -> IO ()
