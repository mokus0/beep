{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BEEP.Core.Word31 (Word31, word31toWord32) where

import Data.Word
import Data.Bits

newtype Word31 = Word31 Word32 deriving (Num, Enum, Bits, Bounded)

mask31 :: Word32
mask31 = bit 31 - 1

word31toWord32 (Word31 x) = x .&. mask31

instance Show Word31 where
    showsPrec p = showsPrec p . word31toWord32
instance Read Word31 where
    readsPrec = mapReadsPrec Word31
mapReadsPrec f p s = fmap (\(x,y) -> (f x, y)) (readsPrec p s)

instance Eq Word31 where
    x == y = word31toWord32 x == word31toWord32 y
instance Ord Word31 where
    compare x y = compare (word31toWord32 x) (word31toWord32 y)
instance Real Word31 where
    toRational = toRational . word31toWord32
instance Integral Word31 where
    quotRem x y = case quotRem (word31toWord32 x) (word31toWord32 y) of
        (q,r) -> (Word31 q, Word31 r)
    divMod  x y = case divMod (word31toWord32 x) (word31toWord32 y) of
        (q,r) -> (Word31 q, Word31 r)
    toInteger = toInteger . word31toWord32
