module Network.BEEP.Mapping.TCP.Seq.Get where

import Network.BEEP.Mapping.TCP.Seq.Types
import Network.BEEP.Core.DataFrame.Get

import Data.Attoparsec.Incremental.Char8
import Data.ByteString.Lazy.Char8

seqFrame = do
    string (pack "SEQ") <?> "SEQ"
    skipBlank
    ch <- channelId
    skipBlank
    ack <- ackno
    skipBlank
    win <- window
    skipBlank
    crlf
    return (Seq ch ack win)

ackno = fmap AckNo seqno

window = fmap Window size
