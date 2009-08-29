module Network.BEEP.Core.DataFrame.Get (getDataFrame) where

import Network.BEEP.Core.Word31
import Data.Word

import Network.BEEP.Core.DataFrame hiding (common)
import Data.ByteString.Lazy.Char8
import Data.Attoparsec.Char8

getDataFrame :: ByteString -> Either String (ByteString, DataFrame)
getDataFrame bs = case parse dataFrame bs of
    (rest, Right frame) -> Right (rest, frame)
    (_   , Left err)    -> Left err
    
dataFrame :: Parser DataFrame
dataFrame = do
    hdr <- header
    dat <- payload (payloadSize hdr)
    trailer
    return (DataFrame hdr dat)

header :: Parser Header
header = do
    hdr <- choice
        [ fmap MsgHdr msg
        , fmap RpyHdr rpy
        , fmap AnsHdr ans
        , fmap ErrHdr err
        , fmap NulHdr nul
        ]
    crlf
    return hdr

{-# INLINE msgLike #-}
msgLike :: String -> (Common -> a) -> Parser a
msgLike tag con = do
    string (pack tag)
    skipBlank
    com <- common
    return (con com)

msg :: Parser Msg
msg = msgLike "MSG" Msg
rpy :: Parser Rpy
rpy = msgLike "RPY" Rpy
ans :: Parser Ans
ans = do
    ans <- msgLike "ASN" Ans
    ansno <- ansno
    return (ans ansno)
err :: Parser Err
err = msgLike "ERR" Err
nul :: Parser Nul
nul = msgLike "NUL" Nul

common :: Parser Common
common = do
    channel <- channel
    skipBlank
    msgno   <- msgno
    skipBlank
    more    <- more
    skipBlank
    seqno   <- seqno
    skipBlank
    size    <- size
    return (Common channel msgno more seqno size)

channel :: Parser Channel
channel = fmap Channel word31

msgno :: Parser MsgNo
msgno = fmap MsgNo word31

more :: Parser More
more = choice
    [ char '*' >> return More
    , char '.' >> return NoMore
    ]

seqno :: Parser SeqNo
seqno = fmap SeqNo word32

size :: Parser Size
size = fmap Size word31

ansno :: Parser AnsNo
ansno = fmap AnsNo word31

word31 :: Parser Word31
word31 = fmap fromInteger integer

word32 :: Parser Word32
word32 = fmap fromInteger integer

payload :: Size -> Parser Payload
payload sz = do
    dat <- takeCount (fromIntegral sz)
    return (Payload dat)

trailer :: Parser ()
trailer = do
    string (pack "END") <?> "END"
    crlf

crlf :: Parser ()
crlf = do
    skipBlank
    char '\r' <?> "CR LF"
    char '\n' <?> "LF"
    return ()

skipBlank :: Parser ()
skipBlank = skipMany (satisfy (inClass " \t"))