module Network.BEEP.Core.DataFrame.Get
    ( module Network.BEEP.Core.DataFrame.Get
    , Result(..)
    ) where

import Network.BEEP.Core.Word31
import Data.Word
import Control.Applicative

import qualified Network.BEEP.Core.DataFrame.Types as DataFrame
import Network.BEEP.Core.DataFrame.Types 
    ( DataFrame(..), Header(..)
    , Common(..)
    , Msg(..), Rpy(..), Ans(..), Err(..), Nul(..)
    , ChannelId(..), MsgNo(..), More(..), SeqNo(..), Size(..), AnsNo(..)
    , withTag1
    )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Attoparsec.Char8 as Atto

getDataFrame :: BS.ByteString -> Result DataFrame
getDataFrame bs = parse dataFrame bs

-- getDataFrameOr :: Parser (Either other DataFrame) other -> ByteString -> Result (Either other DataFrame)
getDataFrameOr other bs = parse (fmap Left other <|> fmap Right dataFrame) bs

dataFrame :: Parser DataFrame
dataFrame = do
    hdr <- header
    dat <- payload (DataFrame.size hdr)
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
msgLike :: (Common -> a) -> String -> Parser a
msgLike con tag = do
    string (BS.pack tag) <?> tag
    skipBlank
    com <- common
    return (con com)

msg :: Parser Msg
msg = withTag1 (msgLike Msg)
rpy :: Parser Rpy
rpy = withTag1 (msgLike Rpy)
ans :: Parser Ans
ans = withTag1 $ \tag -> do
    ans <- msgLike Ans tag
    skipBlank
    ansno <- ansno
    return (ans ansno)
err :: Parser Err
err = withTag1 (msgLike Err)
nul :: Parser Nul
nul = withTag1 (msgLike Nul)

common :: Parser Common
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

channelId :: Parser ChannelId
channelId = fmap ChannelId word31

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
word31 = fmap fromInteger decimal

word32 :: Parser Word32
word32 = fmap fromInteger decimal

payload :: Size -> Parser BL.ByteString
payload sz = do
    dat <- Atto.take (fromIntegral sz)
    return (BL.fromChunks [dat])

trailer :: Parser ()
trailer = do
    string (BS.pack "END") <?> "END"
    crlf

crlf :: Parser ()
crlf = do
    skipBlank
    optional (char '\r' <?> "CR LF")
    char '\n' <?> "LF"
    return ()

skipBlank :: Parser ()
skipBlank = skipMany (satisfy (inClass " \t"))
