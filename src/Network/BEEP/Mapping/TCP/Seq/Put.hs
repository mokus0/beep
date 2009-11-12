module Network.BEEP.Mapping.TCP.Seq.Put where

import Network.BEEP.Mapping.TCP.Seq.Types
import Network.BEEP.Core.DataFrame.Put
import Data.ByteString.Lazy.Char8
import Data.Binary.Put

putSeqFrame = runPut . seqFrame

ackno (AckNo s) = seqno s
window (Window w) = size w

seqFrame frm@(Seq chan ack win) =  do
    putLazyByteString (pack "SEQ")
    space
    channelId chan
    space
    ackno ack
    space
    window win
    crlf

