{-# LANGUAGE GADTs #-}

module Network.BEEP.Core.Session.Types where

import Network.BEEP.Core.DataFrame.Types
import qualified Network.BEEP.Core.Mapping as M
import qualified Network.BEEP.Core.Profile as P
import {-# SOURCE #-} Network.BEEP.Profile.ChannelManagement

import Data.StateRef
import Data.Map (Map)

data Session f m = Session
    { sessionPeerAddr  :: Maybe (M.PeerAddr m)
    , sessionHandle    :: M.PeerHandle m
    , sessionChanZero  :: Channel f m ChannelManagement
    , sessionChannels  :: Ref f (Map ChannelId (SomeChannel f m))
    }

data SomeChannel f m where
    SomeChannel :: P.Profile f m p => Channel f m p -> SomeChannel f m

data Channel f m p = Channel
    { chanSession       :: Session f m
    , chanId            :: ChannelId
    , chanProfile       :: p
    , chanNextMsgOut    :: Ref f MsgNo
    , chanNextSeqOut    :: Ref f SeqNo
    , chanNextSeqIn     :: Ref f SeqNo
    , chanProfileState  :: P.ProfileState p
    }