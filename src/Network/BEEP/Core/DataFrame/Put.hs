module Network.BEEP.Core.DataFrame.Put where

import Network.BEEP.Core.Word31
import Data.Word

import Network.BEEP.Core.DataFrame.Types
    ( DataFrame(..), Header(..)
    , Common(..)
    , Msg(..), Rpy(..), Ans(..), Err(..), Nul(..)
    , ChannelId(..), MsgNo(..), More(..), SeqNo(..), Size(..), AnsNo(..)
    , tagStringOf
    )
import Data.ByteString.Lazy.Char8
import Data.Binary.Put

-- only guaranteed-stable interface is the 'putDataFrame' function
putDataFrame :: DataFrame -> ByteString
putDataFrame = runPut . dataFrame

dataFrame :: DataFrame -> Put
dataFrame (DataFrame hdr dat) = do
    header hdr
    payload dat
    trailer

header :: Header -> Put
header hdr = putIt >> crlf
    where 
        putIt = case hdr of
            MsgHdr hdr -> msg hdr
            RpyHdr hdr -> rpy hdr
            AnsHdr hdr -> ans hdr
            ErrHdr hdr -> err hdr
            NulHdr hdr -> nul hdr

trailer = putLazyByteString (pack "END\r\n")

crlf = putLazyByteString (pack "\r\n")

{-# INLINE msgLike #-}
msgLike :: String -> Common -> Put
msgLike tag com = do
    putLazyByteString (pack tag)
    space
    common com
    
space :: Put
space = putWord8 0x20

msg :: Msg -> Put
msg msg@(Msg com) = msgLike (tagStringOf msg) com

rpy :: Rpy -> Put
rpy rpy@(Rpy com) = msgLike (tagStringOf rpy) com

ans :: Ans -> Put
ans ans@(Ans com (AnsNo ansno)) = do
    msgLike (tagStringOf ans) com
    space
    word31 ansno

err :: Err -> Put
err err@(Err com) = msgLike (tagStringOf err) com

nul :: Nul -> Put
nul nul@(Nul com) = msgLike (tagStringOf nul) com

common :: Common -> Put
common (Common chan msg m s sz) = do
    channelId chan
    space
    msgno msg
    space
    more m
    space
    seqno s
    space
    size sz

channelId (ChannelId c) = word31 c
msgno (MsgNo m) = word31 m
more More   = putWord8 0x2a   {- * -}
more NoMore = putWord8 0x2e   {- . -}
seqno (SeqNo s) = word32 s
size (Size s) = word31 s

word31 :: Word31 -> Put
word31 x = putLazyByteString (pack (show x))

word32 :: Word32 -> Put
word32 x = putLazyByteString (pack (show x))

payload :: ByteString -> Put
payload = putLazyByteString