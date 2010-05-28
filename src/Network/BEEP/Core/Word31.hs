{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BEEP.Core.Word31 (Word31, word31toWord32) where

import Data.Word
import Data.Bits
import Foreign.Storable
import Foreign.Ptr

newtype Word31 = Word31 Word32 deriving (Num, Bits)
instance Enum Word31 where
    fromEnum = fromEnum . word31toWord32
    toEnum   = Word31   . toEnum
instance Bounded Word31 where
    minBound = 0
    maxBound = Word31 mask31
instance Storable Word31 where
    sizeOf = sizeOf . word31toWord32
    alignment = alignment . word31toWord32
    peek = fmap Word31 . peek . castPtr
    peekByteOff p off = fmap Word31 (peekByteOff (castPtr p) off)
    peekElemOff p off = fmap Word31 (peekElemOff (castPtr p) off)
    poke p = poke (castPtr p) . word31toWord32
    pokeByteOff p off = pokeByteOff (castPtr p) off . word31toWord32
    pokeElemOff p off = pokeElemOff (castPtr p) off . word31toWord32
    

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
