{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BEEP.Core.DataFrame where

import Network.BEEP.Core.Word31

import Data.Word (Word32)
import Data.Bits (Bits)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Text.ParserCombinators.ReadP as ReadP

data DataFrame
    = DataFrame Header Payload
    deriving (Eq)

data Header 
    = MsgHdr Msg
    | RpyHdr Rpy
    | AnsHdr Ans
    | ErrHdr Err
    | NulHdr Nul
    deriving (Eq, Show, Read)

newtype Msg = Msg  Common        deriving (Eq, Show, Read)
newtype Rpy = Rpy  Common        deriving (Eq, Show, Read)
data    Ans = Ans !Common !AnsNo deriving (Eq, Show, Read)
newtype Err = Err  Common        deriving (Eq, Show, Read)
newtype Nul = Nul  Common        deriving (Eq, Show, Read)

data Common = Common Channel MsgNo More SeqNo Size deriving (Eq, Show, Read)

newtype Channel = Channel Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype MsgNo   = MsgNo   Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype SeqNo   = SeqNo   Word32 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype Size    = Size    Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype AnsNo   = AnsNo   Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype More    = More    Bool   deriving (Eq, Ord, Read, Show, Bounded, Enum)

newtype Payload = Payload ByteString deriving (Eq, Show, Read)

-- read/show instances for Num types without annoying constructors
instance Show Channel where showsPrec p (Channel x) = showsPrec p x
instance Read Channel where readsPrec = mapReadsPrec Channel

instance Show MsgNo where showsPrec p (MsgNo x) = showsPrec p x
instance Read MsgNo where readsPrec = mapReadsPrec MsgNo

instance Show SeqNo where showsPrec p (SeqNo x) = showsPrec p x
instance Read SeqNo where readsPrec = mapReadsPrec SeqNo

instance Show Size where showsPrec p (Size x) = showsPrec p x
instance Read Size where readsPrec = mapReadsPrec Size

instance Show AnsNo where showsPrec p (AnsNo x) = showsPrec p x
instance Read AnsNo where readsPrec = mapReadsPrec AnsNo

mapReadsPrec f p s = fmap (\(x,y) -> (f x, y)) (readsPrec p s)
