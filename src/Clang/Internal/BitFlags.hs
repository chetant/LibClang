{-# LANGUAGE ScopedTypeVariables #-}

module Clang.Internal.BitFlags
( BitFlags (..)
, orFlags
, andFlags
, unFlags
) where

import Data.Bits
import Data.List

class BitFlags a where
  toBit :: a -> Int

orFlags :: BitFlags a => [a] -> Int
orFlags [] = 0
orFlags fs = foldl1' (.|.) $ map toBit fs

andFlags :: BitFlags a => [a] -> Int
andFlags [] = 0
andFlags fs = foldl1' (.&.) $ map toBit fs

unFlags :: forall a. (BitFlags a, Bounded a, Enum a) => Int -> [a]
unFlags v = filter (checkFlag v) allFlags
  where
    checkFlag i f = (i .&. toBit f) /= 0
    allFlags = enumFrom (minBound :: a)
