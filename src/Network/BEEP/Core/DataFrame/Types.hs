{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}
module Network.BEEP.Core.DataFrame.Types where

import Network.BEEP.Core.Word31

import Data.Word (Word32)
import Data.Bits (Bits)
import Data.ByteString.Lazy.Char8 (ByteString)

data DataFrame
    = DataFrame Header Payload
    deriving (Eq, Show, Read)

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

data Common = Common ChannelId MsgNo More SeqNo Size deriving (Eq, Show, Read)

newtype ChannelId = ChannelId Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype MsgNo   = MsgNo   Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype SeqNo   = SeqNo   Word32 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype Size    = Size    Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
newtype AnsNo   = AnsNo   Word31 deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
data More
    = NoMore
    | More
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

newtype Payload = Payload ByteString deriving (Eq, Show, Read)

-- read/show instances for Num types without annoying constructors
instance Show ChannelId where showsPrec p (ChannelId x) = showsPrec p x
instance Read ChannelId where readsPrec = mapReadsPrec ChannelId

instance Show MsgNo where showsPrec p (MsgNo x) = showsPrec p x
instance Read MsgNo where readsPrec = mapReadsPrec MsgNo

instance Show SeqNo where showsPrec p (SeqNo x) = showsPrec p x
instance Read SeqNo where readsPrec = mapReadsPrec SeqNo

instance Show Size where showsPrec p (Size x) = showsPrec p x
instance Read Size where readsPrec = mapReadsPrec Size

instance Show AnsNo where showsPrec p (AnsNo x) = showsPrec p x
instance Read AnsNo where readsPrec = mapReadsPrec AnsNo

mapReadsPrec f p s = fmap (\(x,y) -> (f x, y)) (readsPrec p s)

-- typesafe tags
newtype Tag t = Tag { tagString :: String }

class HasTag t where tag :: Tag t
instance HasTag Msg where tag = Tag "MSG"
instance HasTag Rpy where tag = Tag "RPY"
instance HasTag Ans where tag = Tag "ANS"
instance HasTag Err where tag = Tag "ERR"
instance HasTag Nul where tag = Tag "NUL"

withTag :: HasTag a => (String -> a) -> a
withTag f = appTag f tag
    where
        appTag :: (String -> a) -> Tag a -> a
        appTag f (Tag t) = f t

withTag1 :: HasTag a => (String -> f a) -> f a
withTag1 f = appTag f tag
    where
        appTag :: (String -> f a) -> Tag a -> f a
        appTag f (Tag t) = f t

tagOf :: HasTag a => a -> Tag a
tagOf a = tag

tagStringOf :: HasTag a => a -> String
tagStringOf = tagString . tagOf

-- getting at the common stuff
class HasCommon c where
    common :: c -> Common

instance HasCommon Common where common = id
instance HasCommon Msg where common (Msg c)   = c
instance HasCommon Rpy where common (Rpy c)   = c
instance HasCommon Ans where common (Ans c _) = c
instance HasCommon Err where common (Err c)   = c
instance HasCommon Nul where common (Nul c)   = c
instance HasCommon Header where 
    common (MsgHdr hdr) = common hdr
    common (RpyHdr hdr) = common hdr
    common (AnsHdr hdr) = common hdr
    common (ErrHdr hdr) = common hdr
    common (NulHdr hdr) = common hdr

payloadSize :: HasCommon c => c -> Size
payloadSize (common -> Common _ _ _ _ sz) = sz
