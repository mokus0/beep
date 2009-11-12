module Network.BEEP.Core.DataFrame.Get
    ( module Network.BEEP.Core.DataFrame.Get
    , Result(..)
    ) where

import Network.BEEP.Core.Word31
import Data.Word

import Network.BEEP.Core.DataFrame.Types 
    ( DataFrame(..), Header(..), Payload(..)
    , Common(..)
    , Msg(..), Rpy(..), Ans(..), Err(..), Nul(..)
    , ChannelId(..), MsgNo(..), More(..), SeqNo(..), Size(..), AnsNo(..)
    , withTag1
    , reportedPayloadSize
    )
import Data.ByteString.Lazy.Char8 as BL
import Data.Attoparsec.Incremental.Char8

getDataFrame :: ByteString -> Result DataFrame
getDataFrame bs = parse dataFrame bs

getDataFrameOr :: Parser (Either other DataFrame) other -> ByteString -> Result (Either other DataFrame)
getDataFrameOr other bs = parse (fmap Left other <|> fmap Right dataFrame) bs

dataFrame :: Parser r DataFrame
dataFrame = do
    hdr <- header
    dat <- payload (reportedPayloadSize hdr)
    trailer
    return (DataFrame hdr dat)

header :: Parser r Header
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
msgLike :: (Common -> a) -> String -> Parser r a
msgLike con tag = do
    string (pack tag) <?> tag
    skipBlank
    com <- common
    return (con com)

msg :: Parser r Msg
msg = withTag1 (msgLike Msg)
rpy :: Parser r Rpy
rpy = withTag1 (msgLike Rpy)
ans :: Parser r Ans
ans = withTag1 $ \tag -> do
    ans <- msgLike Ans tag
    skipBlank
    ansno <- ansno
    return (ans ansno)
err :: Parser r Err
err = withTag1 (msgLike Err)
nul :: Parser r Nul
nul = withTag1 (msgLike Nul)

common :: Parser r Common
common = do
    channel <- channelId
    skipBlank
    msgno   <- msgno
    skipBlank
    more    <- more
    skipBlank
    seqno   <- seqno
    skipBlank
    size    <- size
    return (Common channel msgno more seqno size)

channelId :: Parser r ChannelId
channelId = fmap ChannelId word31

msgno :: Parser r MsgNo
msgno = fmap MsgNo word31

more :: Parser r More
more = choice
    [ char '*' >> return More
    , char '.' >> return NoMore
    ]

seqno :: Parser r SeqNo
seqno = fmap SeqNo word32

size :: Parser r Size
size = fmap Size word31

ansno :: Parser r AnsNo
ansno = fmap AnsNo word31

word31 :: Parser r Word31
word31 = fmap fromInteger integer

word32 :: Parser r Word32
word32 = fmap fromInteger integer

payload :: Size -> Parser r Payload
payload sz = do
    dat <- takeCount (fromIntegral sz)
    return (Payload dat)

trailer :: Parser r ()
trailer = do
    string (pack "END") <?> "END"
    crlf

crlf :: Parser r ()
crlf = do
    skipBlank
    optional (char '\r' <?> "CR LF")
    char '\n' <?> "LF"
    return ()

skipBlank :: Parser r ()
skipBlank = skipMany (satisfy (inClass " \t"))
