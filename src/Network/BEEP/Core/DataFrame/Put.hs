module Network.BEEP.Core.DataFrame.Put where

import Network.BEEP.Core.Word31
import Data.Word

import Network.BEEP.Core.DataFrame.Types hiding (common)
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
common (Common (ChannelId channel) (MsgNo msgno) more (SeqNo seqno) (Size size)) = do
    word31 channel
    space
    word31 msgno
    space
    case more of
        More ->   putWord8 0x2a   {- * -}
        NoMore -> putWord8 0x2e   {- . -}
    space
    word32 seqno
    space
    word31 size

word31 :: Word31 -> Put
word31 x = putLazyByteString (pack (show x))

word32 :: Word32 -> Put
word32 x = putLazyByteString (pack (show x))

payload :: Payload -> Put
payload (Payload bs) = putLazyByteString bs