{-# LANGUAGE
        FlexibleContexts, RecursiveDo
  #-}
module Network.BEEP.Core.Session
    ( Session, Channel
    , initiateSession, createSession
    , disconnectSession, terminateSession
    ) where

import Network.BEEP.Core.Session.Types
import Network.BEEP.Core.DataFrame
import qualified Network.BEEP.Core.Mapping as M
import qualified Network.BEEP.Core.Profile as P
import {-# SOURCE #-} Network.BEEP.Profile.ChannelManagement

import Data.ByteString.Class
import Data.ByteString.Lazy as BL
import Network.URI

import Control.Monad.Fix
import Data.StateRef
import qualified Data.Map as Map

newChannelState session chanId profile = do
    chanState  <- P.initialize profile session
    
    msgOut  <- newRef 0
    seqOut  <- newRef 0
    seqIn   <- newRef 0
    
    return Channel
        { chanSession       = session
        , chanId            = chanId
        , chanProfile       = profile
        , chanProfileState  = chanState
        
        , chanNextMsgOut    = msgOut
        , chanNextSeqOut    = seqOut
        , chanNextSeqIn     = seqIn
        }

initiateSession :: (MonadFix f, HasRef f, M.Mapping f m) => M.PeerAddr m -> [URI] -> f (Session f m)
initiateSession addr profiles = mdo
    handle <- M.initiate addr
    chan0 <- newChannelState session 0 ChannelManagement
    channels <- newRef (Map.singleton 0 (SomeChannel chan0))
    M.newChannel handle 0
    
    let session = Session
            { sessionPeerAddr   = Just addr
            , sessionHandle     = handle
            , sessionChanZero   = chan0
            , sessionChannels   = channels
            }
    
    let greeting = toLazyByteString (Greeting profiles)
    sendReply chan0 0 greeting
    
    return session

createSession :: (MonadFix f, HasRef f,  M.Mapping f m) => M.PeerSpec m -> M.Role -> [URI] -> f (Session f m)
createSession peer role profiles = mdo
    handle <- M.createPeer peer role
    addr   <- M.getPeerAddr handle
    chan0 <- newChannelState session 0 ChannelManagement
    channels <- newRef (Map.singleton 0 (SomeChannel chan0))
    M.newChannel handle 0
    
    let session = Session
            { sessionPeerAddr   = Just addr
            , sessionHandle     = handle
            , sessionChanZero   = chan0
            , sessionChannels   = channels
            }
    
    let greeting = toLazyByteString (Greeting profiles)
    sendReply chan0 0 greeting
    
    return session

disconnectSession :: M.Mapping f m => Session f m -> f ()
disconnectSession session = do
    M.disconnect (sessionHandle session)

terminateSession :: M.Mapping f m => Session f m -> f ()
terminateSession session = do
    M.terminate (sessionHandle session)

-- low level channel state functions
channelGetAndIncrementMsgOut ch = atomicModifyRef (chanNextMsgOut ch) (\a -> (succ a, a))
channelGetAndExtendSeqOut ch sz = atomicModifyRef (chanNextSeqOut ch) (\a -> (extendSeqNo a sz, a))
channelGetAndExtendSeqIn  ch sz = atomicModifyRef (chanNextSeqIn  ch) (\a -> (extendSeqNo a sz, a))

-- low level messaging functions:
sendMessage :: (Monad f, M.Mapping f m) => Channel f m p -> BL.ByteString -> f MsgNo
sendMessage channel msg = do
    msgno <- channelGetAndIncrementMsgOut channel
    seqno <- channelGetAndExtendSeqOut channel (fromIntegral $ BL.length msg)
    let frame = mkMsg (chanId channel) msgno seqno msg
    
    M.send (sessionHandle (chanSession channel)) frame
    return msgno

sendReply, sendError :: (Monad f, M.Mapping f m) => Channel f m p -> MsgNo -> BL.ByteString -> f ()
sendReply channel msgno rpy = do
    seqno <- channelGetAndExtendSeqOut channel (fromIntegral $ BL.length rpy)
    let frame = mkRpy (chanId channel) msgno seqno rpy
    
    M.send (sessionHandle (chanSession channel)) frame

sendError channel msgno err = do
    seqno <- channelGetAndExtendSeqOut channel (fromIntegral $ BL.length err)
    let frame = mkErr (chanId channel) msgno seqno err
    
    M.send (sessionHandle (chanSession channel)) frame

sendAnswer :: (Monad f, M.Mapping f m) => Channel f m p -> MsgNo -> AnsNo -> BL.ByteString -> f ()
sendAnswer channel msgno ansno ans = do
    seqno <- channelGetAndExtendSeqOut channel (fromIntegral $ BL.length ans)
    let frame = mkAns (chanId channel) msgno seqno ansno ans
    
    M.send (sessionHandle (chanSession channel)) frame

sendNull :: (Monad f, M.Mapping f m) => Channel f m p -> MsgNo -> f ()
sendNull channel msgno = do
    seqno <- channelGetAndExtendSeqOut channel 0
    let frame = mkNul (chanId channel) msgno seqno
    
    M.send (sessionHandle (chanSession channel)) frame
    undefined

receiveFrame :: (Monad f, M.Mapping f m) => Channel f m p -> f DataFrame
receiveFrame channel = do
    -- TODO: think about concurrency as it relates to expected values (msgno, seqno, etc)
    -- TODO: receive only on the requested channel!
    frame <- M.receive (sessionHandle (chanSession channel))
    
    expectedSeq <- channelGetAndExtendSeqIn    channel (size frame)
    if expectedSeq /= seqNo frame
        then fail ("Frame received out-of-sequence on channel " ++ show (channelId frame) ++ ": " ++ show frame)
        else return frame