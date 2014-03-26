{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Clang.Internal.BitFlags
( BitFlags (..)
, orFlags
, andFlags
, unFlags
) where

import Data.Bits
import Data.List

class (Bits (FlagInt a), Num (FlagInt a)) => BitFlags a where
  type FlagInt a :: *
  type FlagInt a = Int

  toBit :: a -> FlagInt a

orFlags :: BitFlags a => [a] -> FlagInt a
orFlags [] = 0
orFlags fs = foldl1' (.|.) $ map toBit fs

andFlags :: BitFlags a => [a] -> FlagInt a
andFlags [] = 0
andFlags fs = foldl1' (.&.) $ map toBit fs

unFlags :: forall a. (BitFlags a, Bounded a, Enum a) => FlagInt a -> [a]
unFlags v = filter (checkFlag v) allFlags
  where
    checkFlag i f = (i .&. toBit f) /= 0
    allFlags = enumFrom (minBound :: a)
