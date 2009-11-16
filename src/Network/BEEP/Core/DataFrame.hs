{-# LANGUAGE ViewPatterns #-}
module Network.BEEP.Core.DataFrame
    ( module Network.BEEP.Core.DataFrame.Types
    , getDataFrame, putDataFrame
    
    , payloadSize, reportedPayloadSize
    , extendSeqNo
    , splitCommonAt, splitDataFrameAt
    , mkMsg, mkRpy, mkAns, mkErr, mkNul
    ) where

import Network.BEEP.Core.DataFrame.Types
import Network.BEEP.Core.DataFrame.Get (getDataFrame)
import Network.BEEP.Core.DataFrame.Put (putDataFrame)

import qualified Data.ByteString.Lazy as BL

payloadSize :: DataFrame -> Size
payloadSize (DataFrame hdr (Payload payload)) = fromIntegral (BL.length payload)

reportedPayloadSize :: HasCommon t => t -> Size
reportedPayloadSize (common -> Common _ _ _ _ sz) = sz

extendSeqNo :: SeqNo -> Size -> SeqNo
extendSeqNo (SeqNo seqno) size = SeqNo (seqno + fromIntegral size)

splitCommonAt :: Size -> Common -> (Common, Common)
splitCommonAt sz (Common chan msg more seq cSz) =
    ( Common chan msg More  seq                   p1sz
    , Common chan msg more (extendSeqNo seq p1sz) p2sz
    ) where 
        p1sz = min cSz sz
        p2sz = max 0 (cSz-sz)

splitDataFrameAt :: Size -> DataFrame -> (DataFrame, Maybe DataFrame)
splitDataFrameAt sz frame@(DataFrame hdr (Payload bs))
    | sz == origSz  = (frame, Nothing)
    | otherwise     = (part1, Just part2)
        where
            origSz = payloadSize frame
            sz1 = min sz origSz
            sz2 = origSz - sz
            
            hdr1 = modifyCommon (fst . splitCommonAt sz1) hdr
            hdr2 = modifyCommon (snd . splitCommonAt sz1) hdr
            
            (bs1, bs2) = BL.splitAt (fromIntegral sz) bs
            
            part1 = DataFrame hdr1 (Payload bs1)
            part2 = DataFrame hdr2 (Payload bs2)

mkMsg c m s   bs = DataFrame (MsgHdr (Msg (Common c m NoMore s (fromIntegral $ BL.length bs))  )) (Payload bs)
mkRpy c m s   bs = DataFrame (RpyHdr (Rpy (Common c m NoMore s (fromIntegral $ BL.length bs))  )) (Payload bs)
mkAns c m s a bs = DataFrame (AnsHdr (Ans (Common c m NoMore s (fromIntegral $ BL.length bs)) a)) (Payload bs)
mkErr c m s   bs = DataFrame (ErrHdr (Err (Common c m NoMore s (fromIntegral $ BL.length bs))  )) (Payload bs)
mkNul c m s      = DataFrame (NulHdr (Nul (Common c m NoMore s 0                            )  )) (Payload BL.empty)