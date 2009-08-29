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
    deriving (Eq)
instance Show Header where
    showsPrec p hdr = showIt . showString "\r\n"
        where
            showIt = case hdr of
                MsgHdr msg -> shows msg
                RpyHdr msg -> shows msg
                AnsHdr msg -> shows msg
                ErrHdr msg -> shows msg
                NulHdr msg -> shows msg
instance Read Header where
    readsPrec p = readP_to_S $ choice
        [ fmap MsgHdr (readS_to_P reads)
        , fmap RpyHdr (readS_to_P reads)
        , fmap AnsHdr (readS_to_P reads)
        , fmap ErrHdr (readS_to_P reads)
        , fmap NulHdr (readS_to_P reads)
        ]

readpSimpleHdr tag con = do
    mapM_ char tag
    skipSpaces
    common <- readpCommon
    return (con common)

newtype Msg = Msg  Common deriving (Eq)
instance Show Msg where showsPrec p (Msg common) = showString "MSG " . showsCommon common
instance Read Msg where readsPrec p = readP_to_S (readpSimpleHdr "MSG" Msg)
newtype Rpy = Rpy  Common deriving (Eq)
instance Show Rpy where showsPrec p (Rpy common) = showString "RPY " . showsCommon common
instance Read Rpy where readsPrec p = readP_to_S (readpSimpleHdr "RPY" Rpy)
data    Ans = Ans !Common !AnsNo deriving (Eq)
instance Show Ans where 
    showsPrec p (Ans common ansno) 
        = showString "ANS " . showsCommon common
        . showChar ' ' . shows ansno
instance Read Ans where
    readsPrec p = readP_to_S $ do
        ans <- readpSimpleHdr "ANS" Ans
        ansno <- readS_to_P reads
        return (ans ansno)
newtype Err = Err  Common deriving (Eq)
instance Show Err where showsPrec p (Err common) = showString "ERR " . showsCommon common
instance Read Err where readsPrec p = readP_to_S (readpSimpleHdr "ERR" Err)
newtype Nul = Nul  Common deriving (Eq)
instance Show Nul where showsPrec p (Nul common) = showString "NUL " . showsCommon common
instance Read Nul where readsPrec p = readP_to_S (readpSimpleHdr "NUL" Nul)

crlf = char '\CR' >> char '\LF'

readpTrailer = do
    skipSpaces
    mapM_ char "END"
    many (choice [char ' ', char '\t'])
    crlf
    return ()

data Common = Common Channel MsgNo More SeqNo Size deriving (Eq, Show)
showCommon c = showsCommon c ""
showsCommon (Common channel msgno more seqno size) = unwords
    [shows channel, shows msgno, shows more, shows seqno, shows size]
    where 
        unwords [] = id
        unwords [x] = x
        unwords (x:xs) = x . showChar ' ' . unwords xs
        
readsCommon = readP_to_S readpCommon
readpCommon = do
    skipSpaces
    channel <- readS_to_P reads
    skipSpaces
    msgno   <- readS_to_P reads
    skipSpaces
    more    <- readS_to_P reads
    skipSpaces
    seqno   <- readS_to_P reads
    skipSpaces
    size    <- readS_to_P reads
    return (Common channel msgno more seqno size)

newtype Channel = Channel Word31 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Bits)
instance Show Channel where showsPrec p (Channel x) = showsPrec p x
instance Read Channel where readsPrec = mapReadsPrec Channel

newtype MsgNo   = MsgNo   Word31 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Bits)
instance Show MsgNo where showsPrec p (MsgNo x) = showsPrec p x
instance Read MsgNo where readsPrec = mapReadsPrec MsgNo

newtype SeqNo   = SeqNo   Word32 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Bits)
instance Show SeqNo where showsPrec p (SeqNo x) = showsPrec p x
instance Read SeqNo where readsPrec = mapReadsPrec SeqNo

newtype Size    = Size    Word31 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Bits)
instance Show Size where showsPrec p (Size x) = showsPrec p x
instance Read Size where readsPrec = mapReadsPrec Size

newtype AnsNo   = AnsNo   Word31 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Bits)
instance Show AnsNo where showsPrec p (AnsNo x) = showsPrec p x
instance Read AnsNo where readsPrec = mapReadsPrec AnsNo

newtype More    = More    Bool   deriving (Bounded, Enum, Eq, Ord)
instance Show More where
    showsPrec p (More True ) = showChar '*'
    showsPrec p (More False) = showChar '.'
instance Read More where
    readsPrec p ('*':rest) = [(More True,  rest)]
    readsPrec p ('.':rest) = [(More False, rest)]
    readsPrec p _ = []

newtype Payload = Payload ByteString deriving (Eq)

mapReadsPrec f p s = fmap (\(x,y) -> (f x, y)) (readsPrec p s)

